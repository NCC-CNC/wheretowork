class WeightSolutionChart {

  constructor(data, colors) {
    this.data = data;
    this.width = 120;
    this.height = 110;
    this.chartRadius = this.width / 2;
    this.arcMinRadius = 50;
    this.arcWidth = -20; // this.chartRadius - this.arcMinRadius - 1;
    this.scale = d3
      .scaleLinear()
      .domain([0, 1])
      .range([0, Math.PI]);
    this.createArc = d3
      .arc()
      .innerRadius((_, i) => this.getInnerRadius(i))
      .outerRadius((_, i) => this.getOuterRadius(i))
      .startAngle(this.deg2Rad(270))
      .endAngle((d) => this.scale(d) + this.deg2Rad(270));
    this.locale = {
      current_held: "Current",
      solution_held: "Solution",
      factor: "Factor"
    };
    this.colors = colors
  }

  rad2deg(angle) {
    return angle * 180 / Math.PI;
  }

  deg2Rad(rad) {
    return (Math.PI / 180) * rad;
  }

  getInnerRadius(index) {
    return this.arcMinRadius + (1 - (index + 1)) * this.arcWidth;
  }

  getOuterRadius(index) {
    return this.getInnerRadius(index) + this.arcWidth;
  }

  renderSvg(el) {
    return d3
      .select(el)
      .append("svg")
      .attr("width", this.width)
      .attr("height", this.height * 0.50)
      .append("g")
      .attr("transform", `translate(${this.width / 2},${this.height / 2})`);
  }

  showStats(d, tooltip, type) {
    // factor value
    tooltip
      .append("div")
      .text(() => `Factor: ${Math.round(d.factor)}`);
    // current held stats
    tooltip
      .append("div")
      .text(() =>
        `Included: ${Math.round(d.current_held * 100)}% ` +
        `(${Math.round(d.current_held * d.total_amount).toLocaleString()} ${d.units})`)
      .style("font-weight", type === "current_held" ? "bold" : "normal")
      .style("color", this.colors.current_held);
    // solution held
    tooltip
      .append("div")
      .text(() =>
        `Solution: ${Math.round(d.solution_held * 100)}% ` +
        `(${Math.round(d.solution_held * d.total_amount).toLocaleString()} ` +
        `${d.units || "units"})`)
      .style("font-weight", type === "solution_held" ? "bold" : "normal")
      .style("color", this.colors.solution_held);
  }

  renderArcs(svg, tooltip, type, delay) {
    const self = this;
    svg
      .append("g")
      .selectAll("path")
      .data(this.data)
      .enter()
      .append("path")
      .attr("class", "arc")
      .attr("fill", this.colors[type])
      .style("cursor", "pointer")
      .on("mouseover", function(e, d) {
        // if not rendering total arc, then make arc slightly thicker
        if (type !== "total") {
          d3
            .select(this)
            .attr("stroke", self.colors[type])
            .attr("stroke-width", 4);
        }
        // add tooltip to show statistics
        tooltip
          .style("display", "inline")
          .style("text-align", "left")
          .style("top", `${e.clientY + 5}px`)
          .style("left", `${e.clientX + 5}px`);
        self.showStats(d, tooltip, type)
        // adjust the tooltip position if the tooltip width goes out of the max X window width
        const tooltipWidth = tooltip.node().getBoundingClientRect().width;
        const windowWidth = window.innerWidth;
        if (e.clientX + 5 + tooltipWidth > windowWidth) {
          tooltip.style("left", `${windowWidth - tooltipWidth}px`);
        }
      })
      .on("mouseout", function() {
        d3
          .select(this)
          .attr("stroke", null);
        tooltip
          .style("display", "none")
          .style("top", `${0}px`)
          .style("left", `${0}px`);
        tooltip
          .selectAll("div")
          .remove();
      })
      .transition()
      .delay((_, i) => i * 200)
      .duration(delay)
      .attrTween("d", (d, i) => {
        const interpolate = d3.interpolate(0, d[type]);
        return t => this.createArc(interpolate(t), i);
      });
  }

  renderAllArcs(svg, tooltip) {
    const allowed_keys = {
      current_held: true,
      solution_held: true,
    };
    const data = this.data[0];
    const sortable = [];
    for (const key in data) {
      if (!isNaN(data[key]) && key in allowed_keys) {
        sortable.push([key, data[key]]);
      }
    }
    sortable.sort(function(a, b) { return b[1] - a[1]; });
    sortable.unshift(["total", data["total"]]);

    let delay = 1000;
    for (let i = 0; i < sortable.length; ++i) {
      this.renderArcs(svg, tooltip, sortable[i][0], delay);
      delay += 300;
    }
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

  renderTitle(el) {
    d3.select(el)
      .style("max-width", "110px")
      .style("word-break", "break-word")
      .style("text-align", "center")
      .append("span")
      .attr("class", "provenance-container")
      .append("label")
      .style("color", this.data[0].status ? "black" : "#B8B8B8")
      .node()
      .innerHTML = this.data[0].name;
  }

  render(el) {
    if (this.data.length === 0) return;
    const tooltip = this.renderTooltip(el);
    this.renderTitle(el);
    const svg = this.renderSvg(el);
    this.renderAllArcs(svg, tooltip);
  }

}
