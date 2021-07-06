class WeightResults {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    status,
    factor,
    total_amount,
    current_held,
    solution_held,
    units
  ) {
    // class fields
    this.id = id;
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".weight-results-template")
        .content,
      true);

    const chartContainer = document.createElement('div');
    chartContainer.classList.add('single-theme-result')
    this.el.appendChild(chartContainer);

    const chart = new WeightSolutionChart([{
      name,
      current_held,
      solution_held,
      total_amount,
      units,
      status,
      factor,
    }]);

    chart.render(chartContainer);
  }

  /* render method */
  render() {
    return this.el;
  }

};
