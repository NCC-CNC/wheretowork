class SolutionResults {
  /* constructor */
  constructor(id, container) {
    // set fields
    this.id = id,
    this.solutions = new Array();
    this.el = container;

    // set fields to store HTML elements
    this.select_el =
      this.el.querySelector(`#${id}_select`);
    this.parameters_el =
      this.el.querySelector(".parameters");
    this.statistics_el =
      this.el.querySelector(".statistics");
    this.themes_el =
      this.el.querySelector(".themes");
    this.weights_el =
      this.el.querySelector(".weights");
    this.includes_el =
      this.el.querySelector(".includes");
  }

  /* update methods */
  addSolution(solution) {
    // store solution
    this.solutions.push(solution);
    // create option HTML node for the new solution
    const option = document.createElement("option");
    option.innerText = solution.name;
    option.id = solution.id;
  }

  dropSolution(id) {
    // find solution index
    const idx = this.solutions.findIndex((x) => x.id === id);
    // remove solution from internal memory
    this.solutions.splice(idx, 1);
  }

  showSolution(id) {
    // find solution index
    const idx = this.solutions.findIndex((x) => x.id === id);
    // if solution found, then show it in the widget
    if (idx > -1) {
      // clear solution from panels
      removeAllChildNodes(this.parameters_el);
      removeAllChildNodes(this.statistics_el);
      removeAllChildNodes(this.themes_el);
      removeAllChildNodes(this.weights_el);
      removeAllChildNodes(this.includes_el);
      // update color shown in legends
      document
      .querySelectorAll(".solution-results .legend .legend-solution-symbol")
      .forEach((x) => {
        x.style.backgroundColor = this.solutions[idx].solution_color;
        x.style.color = this.solutions[idx].solution_color;
      });
      // add new solution to solution results container
      this.solutions[idx].render_parameters(this.parameters_el)
      this.solutions[idx].render_statistics(this.statistics_el)
      this.solutions[idx].render_themes(this.themes_el);
      this.solutions[idx].render_weights(this.weights_el);
      this.solutions[idx].render_includes(this.includes_el);
    }
  }

  /* render method */
  render() {
    // none needed
  }

}
