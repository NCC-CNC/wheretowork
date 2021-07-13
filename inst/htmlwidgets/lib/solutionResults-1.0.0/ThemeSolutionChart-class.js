class ThemeSolutionChart {

  constructor(data, colors) {
    this.data = data;
    this.width = 120;
    this.height = 120;
    this.chartRadius = this.width / 2;
    this.arcMinRadius = 1;
    this.arcPadding = 3;
    this.numArcs = data.length;
    this.arcWidth =
      (this.chartRadius - this.arcMinRadius - this.numArcs * this.arcPadding) /
      this.numArcs;
    this.minWordCount = 25;
    this.fontSize = 12;
    this.scale = d3
      .scaleLinear()
      .domain([0, 1])
      .range([0, 2 * Math.PI]);
    this.createArc = d3
      .arc()
      .innerRadius((_, i) => this.getInnerRadius(i))
      .outerRadius((_, i) => this.getOuterRadius(i))
      .startAngle(0)
      .endAngle((d) => this.scale(d));
    this.locale = {
      feature_goal: "Goal",
      feature_current_held: "Current",
      feature_solution_held: "Solution",
    };
    this.colors = colors;
  }

  rad2deg(angle) {
    return angle * 180 / Math.PI;
  }

  getInnerRadius(index) {
    return this.arcMinRadius + (this.numArcs - (index + 1)) * (this.arcWidth + this.arcPadding);
  }

  getOuterRadius(index) {
    return this.getInnerRadius(index) + this.arcWidth;
  }

  minMultArray(x, y) {
    let out = Infinity;
    for (let i = 0; i < x.length; ++i) {
      out = Math.min(out, x[i] * y[i]);
    }
    return Math.round(out);
  }

  renderSvg(el) {
    return d3
      .select(el)
      .append("svg")
      .attr("width", this.width)
      .attr("height", this.height)
      .append("g")
      .attr("transform", `translate(${this.width / 2},${this.height / 2})`);
  }

  renderTooltip(el) {
    return d3.select(el)
      .append("div")
      .style("display", "none")
      .style("background-color", "white")
      .style("border", "solid")
      .style("border-width", "2px")
      .style("border-radius", "5px")
      .style("padding", "5px")
      .style("position", "fixed")
      .style("top", 0)
      .style("left", 0)
  }

  showStats(d_current, d_goal, d_solution, tooltip, locale) {
    // compute status
    /// current stats
    const current_stats =
      `Current: ${Math.round(d_current[1] * 100)}% ` +
      `(${Math.round(d_current[1] * d_current[4])} ${d_current[6]})`;
    /// goal stats
    let goal_stats =
      `Goal: ${Math.round(d_goal[1] * 100)}% ` +
      `(${Math.round(d_goal[1] * d_goal[4])} ${d_goal[6]})`;
    /// solution stats
    const solution_stats =
      `Solution: ${Math.round(d_solution[1] * 100)}% ` +
      `(${Math.round(d_solution[1] * d_solution[4])} ${d_solution[6]})`;
    // add tooltips
    /// current held
    tooltip
      .append("div")
      .text(current_stats)
      .style("color", d_current[2])
      .style("font-weight", d_current[0] === locale ? "bold" : "normal")
    // goal held
    tooltip
      .append("div")
      .text(goal_stats)
      .style("color", d_goal[2])
      .style("font-weight", d_goal[0] === locale ? "bold" : "normal")
    // solution held
    tooltip
      .append("div")
      .text(solution_stats)
      .style("color", d_solution[2])
      .style("font-weight", d_solution[0] === locale ? "bold" : "normal")
  }

  renderArcs(svg, tooltip) {
    const self = this;
    const inner_rings_length = Object.keys(this.colors).length;
    const sorted_data = [];
    for (let i = 0; i < this.data.length; ++i) {
      const datum = this.data[i];
      const sortable = [];
      for (const key in datum) {
        if (!isNaN(datum[key]) && key in this.colors) {
          sortable.push([
            key,
            datum[key],
            this.colors[key],
            datum.feature_name,
            datum.feature_total_amount,
            datum.feature_status,
            datum.units,
          ]);
        }
      }
      sortable.sort(function(a, b) {
        return b[1] - a[1];
      });
      sorted_data.push(sortable);
    }
    for (let j = 0; j < inner_rings_length; ++j) {
      svg
        .append("g")
        .selectAll("path")
        .data(sorted_data)
        .enter()
        .append("path")
        .attr("class", "arc")
        .attr("fill", (d) => { return d[j][2]; })
        .style("cursor", "pointer")
        .on("mouseover", function(e, d) {
          // determine type
          const type = d[j][0];
          // if not rendering total arc, then make arc slightly thicker
          if (type !== "total") {
            let strokeWidth = self.arcWidth * 0.4;
            strokeWidth = strokeWidth > 4 ? 4 : strokeWidth;
            d3
              .select(this)
              .attr("stroke", d[j][2])
              .attr("stroke-width", strokeWidth);
          }
          // add tooltip to show statistics
          tooltip
            .style("display", "inline")
            .style("top", `${e.clientY + 5}px`)
            .style("left", `${e.clientX + 5}px`);
          // show stats in tool tip
          tooltip
            .append("div")
            .text(`${d[j][3]}`)
            .style("color", d[j][5] ? "black" : "#B8B8B8");
          // append tooltip content
          self.showStats(
            d.find((x) => x[0] === "feature_current_held"),
            d.find((x) => x[0] === "feature_goal"),
            d.find((x) => x[0] === "feature_solution_held"),
            tooltip,
            type);
        })
        .on("mouseout", function() {
          d3
            .select(this)
            .attr("stroke", null);
          tooltip
            .style("display", "none")
            .style("top", `${0}px`)
            .style("left", `${0}px`)
          tooltip
            .selectAll("div")
            .remove();
        })
        .transition()
        .delay((_, i) => i * 200)
        .duration(1000)
        .attrTween("d", (d, i) => {
          const interpolate = d3.interpolate(0, d[j][1]);
          return t => this.createArc(interpolate(t), i);
        });
    }
  }

  renderTitle(el, tooltip) {
    const self = this;
    const any_active = self.data.some((x) => x.feature_status);
    d3.select(el)
      .append("label")
      .style("cursor", "pointer")
      .style("max-width", "110px")
      .style("text-align", "center")
      .style("color", any_active ? "black" : "#B8B8B8")
      .on("mouseover", function(e) {
        tooltip
          .style("display", "inline")
          .style("top", `${e.clientY + 5}px`)
          .style("left", `${e.clientX + 5}px`)
          .append("div")
          .text("Summary of theme")
        const parsedData = {
          feature_current_held: [],
          feature_goal: [],
          feature_solution_held: [],
          feature_total_amount: [],
        };
        for (const datum of self.data) {
          for (const key in parsedData) {
            parsedData[key].push(datum[key]);
          }
        }
        const attrs =
          ["feature_current_held", "feature_goal", "feature_solution_held"];
        for (let i = attrs.length; i > 0; --i) {
          const attr = attrs[attrs.length - i];
          const m = self.minMultArray(
              parsedData[attr], parsedData.feature_total_amount);
          tooltip
            .append("div")
            .text(() =>
              `${self.locale[attr]}: ≥ ` +
              `${Math.round(Math.min(...parsedData[attr]) * 100)}% `+
              `(≥${m} ${self.data[0].units || "units"})`)
            .style("color", self.colors[attr]);
        }
      })
      .on("mouseout", function() {
        d3
          .select(this)
          .attr("stroke", null);
        tooltip
          .style("display", "none")
          .style("top", `${0}px`)
          .style("left", `${0}px`)
        tooltip
          .selectAll("div")
          .remove();
      })
      .node()
      .innerHTML = this.data[0].name;
  }

  renderAllArcs(svg, tooltip) {
    this.renderArcs(svg, tooltip);
  }

  render(el) {
    const tooltip = this.renderTooltip(el)
    this.renderTitle(el, tooltip);
    const svg = this.renderSvg(el, tooltip);
    this.renderAllArcs(svg, tooltip);
  }

}
