class SingleThemeResults {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    feature_name,
    feature_id,
    feature_status,
    feature_total_amount,
    feature_current_held,
    feature_goal,
    feature_solution_held,
    units,
    mandatory,
    round,
    icon
  ) {
    this.id = id;
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector('.single-theme-results-template')
        .content,
      true);

    const chartContainer = document.createElement('div');
    chartContainer.classList.add('single-theme-result')
    this.el.appendChild(chartContainer);

    const chart = new SingleSolutionChart([{
      name,
      feature_name,
      feature_goal,
      feature_current_held,
      feature_solution_held,
      feature_total_amount
    }]);

    chart.render(chartContainer);

    const l = document.createElement('label');
    l.innerText = name;
    chartContainer.appendChild(l);
  }

  /* render method */
  render() {
    return this.el;
  }

};
