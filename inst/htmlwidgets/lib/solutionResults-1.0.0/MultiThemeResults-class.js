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
        .querySelector(".multi-theme-results-template")
        .content,
      true);

    const l = document.createElement("label");
    this.el.appendChild(l);

    const chart = new MultiSolutionChart(feature_name.map((name, index) => ({
      feature_name: name,
      feature_goal: feature_goal[index],
      feature_current_held: feature_current_held[index],
      feature_solution_held: feature_solution_held[index],
      feature_total_amount: feature_total_amount[index],
    })));

    chart.render(this.el);
  }

  /* render method */
  render() {
    return this.el;
  }

};
