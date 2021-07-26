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
    icon,
    solution_color
  ) {
    // declare fields
    this.id = id;

    // create HTML element
    this.el = document.createElement("div");
    this.el.classList.add("single-theme-result");

    // update goal
    /// manually coerce goal to zero if status is false
    let manual_feature_goal = feature_status ? feature_goal : 0;
    manual_feature_goal -= manual_feature_goal > 1.0e-5 ? 1.0e-5 : 0;

    // update solution held
    let manual_feature_solution_held = feature_solution_held;
    manual_feature_solution_held -=
      manual_feature_solution_held > 1.0e-5 ? 1.0e-5 : 0;

    // create chart
    const chart = new ThemeSolutionChart(
      [{
        name,
        feature_name: name, // show theme name to avoid confusion
        feature_goal: manual_feature_goal,
        feature_current_held,
        feature_solution_held: manual_feature_solution_held,
        feature_total_amount,
        feature_status,
        total: 1,
        units,
      }], {
        feature_goal: "#118ab2",
        feature_current_held: "#06d6a0",
        feature_solution_held: solution_color,
        total: "#cccccc44"
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
