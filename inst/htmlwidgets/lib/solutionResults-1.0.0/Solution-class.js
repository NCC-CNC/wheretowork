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

    // create HTML element to display all solution results
    this.el = document.createElement("div");

    // statistics results
    const statistics_el = document.createElement("div");;
    statistics.forEach((x) => {
      statistics_el.appendChild(newStatistic(manager, x).render());
    });

    // theme results
    const theme_results_el = document.createElement("div");
    theme_results_el.classList.add('solution-theme-results')
    theme_results.forEach((x) => {
      theme_results_el.appendChild(newThemeResults(manager, x).render());
    });

    // weight results
    const weight_results_el = document.createElement("div");
    weight_results.forEach((x) => {
      weight_results_el.appendChild(newWeightResults(manager, x).render());
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
  }

}
