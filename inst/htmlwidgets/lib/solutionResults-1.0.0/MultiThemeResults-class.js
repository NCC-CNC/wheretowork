class MultiThemeResults {
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
        .querySelector('.multi-theme-results-template')
        .content,
      true);

    const chartContainer = document.createElement('div');
    chartContainer.classList.add('multi-theme-result')
    this.el.appendChild(chartContainer);

    const chart = new MultiSolutionChart(feature_name.map((feature_name, index) => ({
      name,
      feature_name,
      feature_goal: feature_goal[index],
      feature_current_held: feature_current_held[index],
      feature_solution_held: feature_solution_held[index],
      feature_total_amount: feature_total_amount[index],
    })));

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
