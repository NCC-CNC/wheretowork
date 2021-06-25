class SolutionSettings {
  /* constructor */
  constructor(id, container, themes, weights, includes) {
    // set container
    this.id = id,
    this.container = container;

    // initialize themes
    this.themes = themes.map((x) => newThemeSetting(id, x));

    // initialize weights
    this.weights = weights.map((x) => newWeightSetting(id, x));

    // initialize includes
    this.includes = includes.map((x) => newIncludeSetting(id, x));
  }

  /* update method */
  updateSetting(id, parameter, value, type) {
    if (type === "theme") {
      const pos = this.themes.findIndex((x) => x.id === id);
      if (pos < 0) {
        console.warn(
          `SolutionSettings.updateSetting(...) failed due to ` +
          `no theme with id: ${id}`);
      } else {
        this.themes[pos].updateParameter(parameter, value);
      }
    } else if (type === "weight") {
      const pos = this.weights.findIndex((x) => x.id === id);
      if (pos < 0) {
        console.warn(
          `SolutionSettings.updateSetting(...) failed due to ` +
          `no weight with id: ${id}`);
      } else {
        this.weights[pos].updateParameter(parameter, value);
      }
    }
  }

  /* render method */
  render() {
    // themes
    const theme_panel = this.container.querySelector(".themes");
    this.themes.forEach((x) => theme_panel.appendChild(x.render()))

    // weights
    const weight_panel = this.container.querySelector(".weights");
    this.weights.forEach((x) => weight_panel.appendChild(x.render()));

    // includes
    const include_panel = this.container.querySelector(".includes");
    this.includes.forEach((x) => include_panel.appendChild(x.render()));

   // initialize tooltips in widget
   $(this.container).find('[data-toggle="tooltip"]').tooltip();
  }
}
