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
    feature_provenance,
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
    const adj_feature_goal = feature_goal.map((x, i) => {
      // manually coerce goals to zero if status is false
      return feature_status[i] ? x : 0;
    });

    // create padded versions of the data to plot
    let plot_feature_goal = adj_feature_goal.slice();
    let plot_feature_current_held = feature_current_held.slice();
    let plot_feature_solution_held = feature_solution_held.slice();
    for (let i = 0; i < plot_feature_goal.length; ++i) {
      // fix overlap issues when there are ties
      /// add padding to goal if goal == current
      if (Math.abs(adj_feature_goal[i] - feature_current_held[i]) < 0.01) {
        plot_feature_goal[i] += 0.01;
      }
      /// add padding to solution if solution == current
      if (Math.abs(feature_solution_held[i] - feature_current_held[i]) < 0.01) {
        plot_feature_solution_held[i] += 0.01;
      }
      /// add padding to solution if solution == goal
      if (Math.abs(feature_solution_held[i] - adj_feature_goal[i]) < 0.01) {
        plot_feature_solution_held[i] += 0.01;
      }

      // clamp maximum values for plotted values
      plot_feature_current_held[i] = Math.min(
        plot_feature_current_held[i], 0.9998
      );
      plot_feature_goal[i] = Math.min(
        plot_feature_goal[i], 0.9998
      );
      plot_feature_solution_held[i] = Math.min(
        plot_feature_solution_held[i], 1.0
      );
    }

    // create chart
    const chart = new ThemeSolutionChart(
      feature_name.map((feature_name, index) => ({
        name,
        feature_name,
        feature_goal: [
          adj_feature_goal[index], plot_feature_goal[index]
        ],
        feature_current_held: [
          feature_current_held[index], plot_feature_current_held[index]
        ],
        feature_solution_held: [
          feature_solution_held[index], plot_feature_solution_held[index]
        ],
        feature_total_amount: feature_total_amount[index],
        feature_status: feature_status[index],
        total: [1.0, 1.0],
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

    // add provenance
    const unique_provs = getUniqueBy(feature_provenance, "name");
    let prov_el = this.el.querySelector(".provenance-container label");
    unique_provs.forEach((x) => createProvenance(prov_el, x));
    $(this.el).find('[data-toggle="tooltip"]').tooltip()

  }

  /* render method */
  render() {
    return this.el;
  }

};
