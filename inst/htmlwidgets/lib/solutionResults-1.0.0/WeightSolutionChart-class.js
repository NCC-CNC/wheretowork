class WeightSolutionChart {

  constructor(data, colors) {
    this.data = data;
    this.width = 180;
    this.height = 120;
    this.colors = colors
  }

  renderSvg(el) {
    return d3
      .select(el)
      .append('svg')
      .attr('width', this.width)
      .attr('height', this.height * 0.60)
      .append('g');
  }

  renderTitle(el) {
    const text = d3.select(el)
      .append('label')
      .style('margin-bottom', '15px');
    text
      .node()
      .innerHTML = this.data[0].name;
    const d = this.data[0];
    if (!d.status) {
      text.attr('class', 'text-muted');
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
      .style('left', 0);
  }

  showStats(d, tooltip, type) {
    tooltip
      .append('div')
      .text(() =>
        `Current: ${Math.round(d.current_held * 100)}% ${Math.round(d.current_held * d.total_amount)} ${d.units
            || 'units'}`
      )
      .style('font-weight', type === 'current' ? 'bold' : 'normal')
      .style('color', this.colors.current);
    tooltip
      .append('div')
      .text(() => `Factor: ${
          d.status
          ? d.factor
          : 0
      }`);
    tooltip
      .append('div')
      .text(() =>
          `Solution: ${Math.round(d.solution_held * 100)}% ${Math.round(d.solution_held * d.total_amount)} ${d.units
              || 'units'}`)
      .style('font-weight', type === 'solution' ? 'bold' : 'normal')
      .style('color', this.colors.solution);
  }

  renderCurrentBar(svg, tooltip) {
    const self = this;
    svg
      .append('g')
      .selectAll('path')
      .data(this.data)
      .enter()
      .append('rect')
      .attr('width', d => d.current_held * this.width)
      .attr('height', this.height * 0.30)
      .attr('fill', self.colors.current)
      .on('mouseover', function(e, d) {
        const strokeWidth = 1.5;
        d3
          .select(this)
          .attr('stroke', self.colors.current)
          .attr('stroke-width', strokeWidth);
        tooltip
          .style('display', 'inline')
          .style('top', `${e.clientY + 5}px`)
          .style('left', `${e.clientX + 5}px`)
        self.showStats(d, tooltip, 'current')
      })
      .on('mouseout', function() {
        d3
          .select(this)
          .attr('stroke', null);
        tooltip
          .style('display', 'none')
          .style('top', `${0}px`)
          .style('left', `${0}px`)
        tooltip
          .selectAll('div')
          .remove();
      })
      .attr('cursor', 'pointer');
  }

  renderSolutionBar(svg, tooltip) {
    const self = this;
    svg
      .append('g')
      .selectAll('path')
      .data(this.data)
      .enter()
      .append('rect')
      .attr('width', d => d.solution_held * this.width)
      .attr('height', this.height * 0.30)
      .attr('fill', self.colors.solution)
      .on('mouseover', function(e, d) {
        const strokeWidth = 1.5;
        d3
          .select(this)
          .attr('stroke', self.colors.solution)
          .attr('stroke-width', strokeWidth);
        tooltip
          .style('display', 'inline')
          .style('top', `${e.clientY + 5}px`)
          .style('left', `${e.clientX + 5}px`)
        self.showStats(d, tooltip, 'solution')
      })
      .on('mouseout', function() {
        d3
          .select(this)
          .attr('stroke', null);
        tooltip
          .style('display', 'none')
          .style('top', `${0}px`)
          .style('left', `${0}px`)
        tooltip
          .selectAll('div')
          .remove();
      })
      .attr('cursor', 'pointer');
  }

  renderBars(svg, tooltip) {
      const d = this.data[0];
      const solution = d.solution_held * d.total_amount;
      const current = d.current_held * d.total_amount;
      if (solution > current) {
        this.renderSolutionBar(svg, tooltip);
        this.renderCurrentBar(svg, tooltip);
      } else {
        this.renderCurrentBar(svg, tooltip);
        this.renderSolutionBar(svg, tooltip);
      }
  }

  render(el) {
    if (this.data.length === 0) return;
    const tooltip = this.renderTooltip(el);
    this.renderTitle(el);
    const svg = this.renderSvg(el);
    this.renderBars(svg, tooltip);
  }

}
