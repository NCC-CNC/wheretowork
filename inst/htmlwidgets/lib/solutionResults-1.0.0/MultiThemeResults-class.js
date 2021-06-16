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
    // class fields
    this.id = id;
    this.chart = undefined;
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".multi-theme-results-template")
        .content,
      true);

    // prepare HTML element
    /// assign id to chart HTML element
    this.el.querySelector(".solution-result").id = id;

    // TODO: add code to display results for the object
    // The current code just inserts a TODO into the DOM
    const l = document.createElement("label");
    l.innerText = "TODO";
    this.el.appendChild(l);
  }

  /* render method */
  render() {
    return this.el;
  }

  /* post render method */
  postrender() {
    // TODO: see ./SingleThemeResults-class.js for example
  }

  /* destroy method */
  destroy() {
    // TODO: see ./SingleThemeResults-class.js for example
  }

};
