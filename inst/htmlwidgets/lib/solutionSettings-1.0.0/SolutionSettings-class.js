class SolutionSettings {
  /* constructor */
  constructor(id, container, themes, weights, includes, parameters) {
    // set container
    this.id = id,
    this.container = container;

    // initialize themes
    this.themes = themes.map((x) => newThemeSetting(id, x));

    // initialize weights
    this.weights = weights.map((x) => newWeightSetting(id, x));

    // initialize includes
    this.includes = includes.map((x) => newIncludeSetting(id, x));

    // initialize parameters
    this.parameters = parameters.map((x) => newParameterSetting(id, x));
  }

  /* update method */
  updateSetting(id, setting, value, type) {
    if (type === "theme") {
      const pos = this.themes.findIndex((x) => x.id === id);
      if (pos < 0) {
        console.warn(
          `SolutionSettings.updateSetting(...) failed due to ` +
          `no theme with id: ${id}`);
      } else {
        this.themes[pos].updateSetting(setting, value);
      }
    } else if (type === "weight") {
      const pos = this.weights.findIndex((x) => x.id === id);
      if (pos < 0) {
        console.warn(
          `SolutionSettings.updateSetting(...) failed due to ` +
          `no weight with id: ${id}`);
      } else {
        this.weights[pos].updateSetting(setting, value);
      }
    } else if (type === "include") {
      const pos = this.includes.findIndex((x) => x.id === id);
      if (pos < 0) {
        console.warn(
          `SolutionSettings.updateSetting(...) failed due to ` +
          `no include with id: ${id}`);
      } else {
        this.includes[pos].updateSetting(setting, value);
      }
    } else if (type === "parameter") {
      const pos = this.parameters.findIndex((x) => x.id === id);
      if (pos < 0) {
        console.warn(
          `SolutionSettings.updateSetting(...) failed due to ` +
          `no parameter with id: ${id}`);
      } else {
        this.parameters[pos].updateSetting(setting, value);
      }
    } else {
      console.warn(`no setting found with id: ${id}`);
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

    // parameters
    const parameter_panel = this.container.querySelector(".parameters");
    this.parameters.forEach((x) => parameter_panel.appendChild(x.render()));

   // initialize tooltips in widget
   $(this.container).find('[data-toggle="tooltip"]').tooltip();
  }
}
