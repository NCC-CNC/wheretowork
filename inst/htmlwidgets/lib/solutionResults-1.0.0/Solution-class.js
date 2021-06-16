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
    this.statistics =
      statistics.map((x) => newStatistic(manager, x));
    this.theme_results =
      theme_results.map((x) => newThemeResults(manager, x));
    this.weight_results =
      weight_results.map((x) => newWeightResults(manager, x));

    // create HTML element to display all solution results
    this.el = document.createElement("div");

    // statistics results
    const statistics_el = document.createElement("div");;
    this.statistics.forEach((x) => {
      return statistics_el.appendChild(x.render());
    })

    // theme results
    const theme_results_el = document.createElement("div");
    this.theme_results.forEach((x) => {
      return theme_results_el.appendChild(x.render());
    });

    // weight results
    const weight_results_el = document.createElement("div");
    this.weight_results.forEach((x) => {
      return weight_results_el.appendChild(x.render());
    });

    // append all HTML  versions of the results objects to HTML element
    this.el.appendChild(statistics_el);
    this.el.appendChild(theme_results_el);
    this.el.appendChild(weight_results_el);
  }

  /* render method */
  render(parent) {
    // append the HTML element to parent element
    parent.appendChild(this.el);

    // call post-render methods to render charts
    this.statistics.forEach((x) => x.postrender());
    this.theme_results.forEach((x) => x.postrender());
    this.weight_results.forEach((x) => x.postrender());
  }

}
