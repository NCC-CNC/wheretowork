class Solution {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    statistics,
    theme_results,
    weight_results
  ) {
    // set fields
    this.id = id;
    this.name = name;

    // statistics results
    this.statistics_el = document.createElement("div");
    statistics.forEach((x) => {
      this.statistics_el.appendChild(newStatistic(manager, x).render());
    });

    // theme results
    this.themes_el = document.createElement("div");
    theme_results.forEach((x) => {
      this.themes_el.appendChild(newThemeResults(manager, x).render());
    });

    // weight results
    this.weights_el = document.createElement("div");
    weight_results.forEach((x) => {
      this.weights_el.appendChild(newWeightResults(manager, x).render());
    });
  }

  /* render method */
  render_statistics(el) {
    el.appendChild(this.statistics_el);
  }

  render_themes(el) {
    el.appendChild(this.themes_el);
  }

  render_weights(el) {
    el.appendChild(this.weights_el);
  }

}
