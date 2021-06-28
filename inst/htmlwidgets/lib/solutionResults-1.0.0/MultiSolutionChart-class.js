class MultiSolutionChart {

  constructor(data) {
    this.data = data;
    this.width = 180;
    this.height = 180;
    this.chartRadius = this.width / 2;
    this.arcMinRadius = 1;
    this.arcPadding = 3;
    this.numArcs = data.length;
    this.arcWidth = (this.chartRadius - this.arcMinRadius - this.numArcs * this.arcPadding) / this.numArcs;
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
      .append('svg')
      .attr('width', this.width)
      .attr('height', this.height)
      .append('g')
      .attr('transform', `translate(${this.width / 2},${this.height / 2})`);
  }

  renderArcs(svg) {
    const self = this;
    const inner_rings_length = Object.keys(this.colors).length;
    const sorted_data = [];
    for (let i = 0; i < this.data.length; ++i) {
      const datum = this.data[i];
      const sortable = [];
      for (const key in datum) {
        if (!isNaN(datum[key]) && key in this.colors) {
          sortable.push([key, datum[key], this.colors[key], datum.feature_name, datum.feature_total_amount]);
        }
      }
      sortable.sort(function(a, b) {
        return b[1] - a[1];
      });
      sorted_data.push(sortable);
    }
    for (let j = 0; j < inner_rings_length; ++j) {
      svg
        .append('g')
        .selectAll('path')
        .data(sorted_data)
        .enter()
        .append('path')
        .attr('class', 'arc')
        .attr('fill', (d) => {
          return d[j][2];
        })
        .style('cursor', 'pointer')
        .on('mouseover', function(_, d) {
          let strokeWidth = self.arcWidth * 0.4;
          strokeWidth = strokeWidth > 3 ? 3 : strokeWidth;
          d3
            .select(this)
            .attr('stroke', d[j][2])
            .attr('stroke-width', strokeWidth);
        })
        .on('mouseout', function() {
          d3
            .select(this)
            .attr('stroke', null);
        })
        .transition()
        .delay((_, i) => i * 200)
        .duration(1000)
        .attrTween('d', (d, i) => {
          const interpolate = d3.interpolate(0, d[j][1]);
          return t => this.createArc(interpolate(t), i);
        });
    }
  }

  renderAllArcs(svg) {
    this.renderArcs(svg);
  }

  render(el) {
    const svg = this.renderSvg(el);
    this.renderAllArcs(svg);
  }

}
