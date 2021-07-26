class Solution {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    parameters,
    statistics,
    theme_results,
    weight_results,
    include_results,
    solution_color
  ) {
    // set fields
    this.id = id;
    this.name = name;

    // parameters results
    this.parameters_el = document.createElement("div");
    this.parameters_el.classList.add("parameters-results");
    this.parameters_el.appendChild(
      newParameters(manager, parameters).render()
    );

    // statistics results
    this.statistics_el = document.createElement("div");
    this.statistics_el.classList.add("statistics-results");
    this.statistics_el.appendChild(
      newStatistics(manager, statistics).render()
    );

    // theme results
    this.themes_el = document.createElement("div");
    this.themes_el.classList.add("themes-results");
    theme_results.forEach((x) => {
      this.themes_el.appendChild(
        newThemeResults(manager, x, solution_color).render());
    });

    // weight results
    this.weights_el = document.createElement("div");
    this.weights_el.classList.add("weights-results");
    if (weight_results.length > 0) {
      weight_results.forEach((x) => {
        this.weights_el.appendChild(
          newWeightResults(manager, x, solution_color).render());
      });
    } else {
      this.weights_el.appendChild(
        document.importNode(
          document
          .getElementById(manager)
          .querySelector(".no-weights-template")
          .content,
        true)
      );
    }

    // include results
    this.includes_el = document.createElement("div");
    this.includes_el.classList.add("includes-results");
    if (include_results.length > 0) {
      include_results.forEach((x) => {
        this.includes_el.appendChild(
          newIncludeResults(manager, x, solution_color).render());
      });
    } else {
      this.includes_el.appendChild(
        document.importNode(
          document
          .getElementById(manager)
          .querySelector(".no-includes-template")
          .content,
        true)
      );
    }
  }

  /* render method */
  render_parameters(el) {
    el.appendChild(this.parameters_el);
  }

  render_statistics(el) {
    el.appendChild(this.statistics_el);
  }

  render_themes(el) {
    el.appendChild(this.themes_el);
  }

  render_weights(el) {
    el.appendChild(this.weights_el);
  }

  render_includes(el) {
    el.appendChild(this.includes_el);
  }

}
