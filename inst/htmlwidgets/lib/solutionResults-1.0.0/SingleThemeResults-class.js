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
        .querySelector(".single-theme-results-template")
        .content,
      true);

    const l = document.createElement("label");
    this.el.appendChild(l);

    const chart = new SingleSolutionChart([{
      feature_name,
      feature_goal,
      feature_current_held,
      feature_solution_held,
      feature_total_amount
    }]);

    chart.render(this.el);
  }

  /* render method */
  render() {
    return this.el;
  }

};
