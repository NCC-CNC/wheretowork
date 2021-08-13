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
    let adj_feature_goal = feature_status ? feature_goal : 0;

    // create padded versions of the data to plot
    let plot_feature_goal = adj_feature_goal;
    let plot_feature_current_held = feature_current_held;
    let plot_feature_solution_held = feature_solution_held;

    // fix overlap issues when there are ties
    /// add padding to goal if goal == current
    if (Math.abs(feature_goal - feature_current_held) < 0.01) {
      plot_feature_goal += 0.01;
    }
    /// add padding to solution if solution == current
    if (Math.abs(feature_solution_held - feature_current_held) < 0.01) {
      plot_feature_solution_held += 0.01;
    }
    /// add padding to solution if solution == goal
    if (Math.abs(feature_solution_held - feature_goal) < 0.01) {
      plot_feature_solution_held += 0.01;
    }

    // clamp maximum values for plotted valuessolutions
    plot_feature_current_held = Math.min(plot_feature_current_held, 0.9998);
    plot_feature_goal = Math.min(plot_feature_goal, 0.9998);
    plot_feature_solution_held = Math.min(plot_feature_solution_held, 1.0);

    // create chart
    const chart = new ThemeSolutionChart(
      [{
        name,
        feature_name: name, // show theme name to avoid confusion
        feature_goal: [
          adj_feature_goal,
          plot_feature_goal
        ],
        feature_current_held: [
          feature_current_held,
          plot_feature_current_held
        ],
        feature_solution_held: [
          feature_solution_held,
          plot_feature_solution_held
        ],
        feature_total_amount,
        feature_status,
        total: [1, 1],
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
