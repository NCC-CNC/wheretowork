class SingleSolutionChart {

  constructor(data) {
    this.data = data;
    this.width = 180;
    this.height = 200;
    this.chartRadius = this.width / 2;
    this.arcMinRadius = 50;
    this.arcWidth = this.chartRadius - this.arcMinRadius - 1 - 10;
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
      feature_goal: 'Goal',
      feature_current_held: 'Current',
      feature_solution_held: 'Solution',           
    };
    const palette = d3.scaleOrdinal(d3.schemeCategory10);
    this.colors = {
      feature_goal: palette(0),
      feature_current_held: palette(1),
      feature_solution_held: palette(2),
    };
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
      .append('svg')
      .attr('width', this.width)
      .attr('height', this.height * 0.60)
      .append('g')
      .attr('transform', `translate(${this.width / 2},${this.height / 2})`);
  }

  renderArcs(svg, tooltip, type, delay) {
    const self = this;
    svg
      .append('g')
      .selectAll('path')
      .data(this.data)
      .enter()
      .append('path')
      .attr('class', 'arc')
      .attr('fill', this.colors[type])
      .style('cursor', 'pointer')
      .on('mouseover', function(e, d) {
        const strokeWidth = 3;
        d3
          .select(this)
          .attr('stroke', self.colors[type])
          .attr('stroke-width', strokeWidth);
        tooltip
          .style('display', 'inline')
          .style('top', `${e.clientY + 5}px`)
          .style('left', `${e.clientX + 5}px`)

        for (const key in self.locale) {
          const locale = self.locale[key];
          tooltip.
            append('div')
            .text(
              locale === 'Goal'
              ? (
                d.feature_status
                ? `${locale}: ${Math.round(d[key] * 100)}% (${Math.round(d[key] * d.feature_total_amount)} ${d.units
                    || 'units'})`
                : `${locale}: 0% (0 ${d.units})`
              )
              : `${locale}: ${Math.round(d[key] * 100)}% (${Math.round(d[key] * d.feature_total_amount)} ${d.units
                  || 'units'})` 
            )
            .style('color', self.colors[key])
            .style('font-weight', type === key ? 'bold' : 'normal')
        }
      })
      .on('mouseout', function() {
        d3
          .select(this)
          .attr('stroke', null);
        tooltip
          .style('display', 'none')
          .style('top', `${0}px`)
          .style('left', `${0}px`);
        tooltip
          .selectAll('div')
          .remove();
      })
      .transition()
      .delay((_, i) => i * 200)
      .duration(delay)
      .attrTween('d', (d, i) => {
        const interpolate = d3.interpolate(0, d[type]);
        return t => this.createArc(interpolate(t), i);
      });
  }

  renderAllArcs(svg, tooltip) {
    const allowed_keys = {
      feature_goal: true,
      feature_current_held: true,
      feature_solution_held: true,
    };
    const data = this.data[0];
    const sortable = [];
    for (const key in data) {
      if (!isNaN(data[key]) && key in allowed_keys) {
        sortable.push([key, data[key]]);
      }
    }
    sortable.sort(function(a, b) { return b[1] - a[1];
    });
    let delay = 1000;
    for (let i = 0; i < sortable.length; ++i) {
      this.renderArcs(svg, tooltip, sortable[i][0], delay);
      delay += 300;
    }
  }

  renderTooltip(el) {
    return d3.select(el)
      .append('div')
      .style('display', 'none')
      .style('background-color', 'white')
      .style('border', 'solid')
      .style('border-width', '2px')
      .style('border-radius', '5px')
      .style('padding', '5px')
      .style('position', 'fixed')
      .style('top', 0)
      .style('left', 0)
  }

  renderTitle(el) {
    d3.select(el)
      .append('label')
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
