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

    // update goals
    const manual_feature_goal = feature_goal.map((x, i) => {
      // manually coerce goals to zero if status is false
      let out = feature_status[i] ? x : 0;
      // fiddle goal to ensure correct bolding in widget
      out -= out > 1.0e-5 ? 1.0e-5 : 0;
      // return result
      return out
    });

    // update feature held
    const manual_feature_solution_held = feature_solution_held.map((x, i) => {
      // fiddle goal to ensure correct bolding in widget
      return x > 1.0e-5 ? x - 1.0e-5 : x;
    });

    // create chart
    const chart = new ThemeSolutionChart(
      feature_name.map((feature_name, index) => ({
        name,
        feature_name,
        feature_goal: manual_feature_goal[index],
        feature_current_held: feature_current_held[index],
        feature_solution_held: manual_feature_solution_held[index],
        feature_total_amount: feature_total_amount[index],
        feature_status: feature_status[index],
        total: 1,
        units,
      })), {
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
