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
    icon,
    solution_color
  ) {
    // declare fields
    this.id = id;

    // create HTML element
    this.el = document.createElement("div");
    this.el.classList.add("multi-theme-result");

    // create chart
    const chart = new MultiSolutionChart(
      feature_name.map((feature_name, index) => ({
        name,
        feature_name,
        feature_goal: feature_goal[index],
        feature_current_held: feature_current_held[index],
        feature_solution_held: feature_solution_held[index],
        feature_total_amount: feature_total_amount[index],
        feature_status: feature_status[index],
        units,
      })), {
        feature_goal: "#118ab2",
        feature_current_held: "#06d6a0",
        feature_solution_held: solution_color,
      }
    );

    // render chart on HTML element
    chart.render(this.el);
  }

  /* render method */
  render() {
    return this.el;
  }

};
