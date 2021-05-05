class SolutionSettings {
  /* constructor */
  constructor(id, container, themes, weights) {
    // set container
    this.id = id,
    this.container = container;
    // initialize themes
    this.themes = themes.map((x) => newThemeSetting(id, x));
    // initialize weights
    this.weights = weights.map((x) => newWeightSetting(id, x));
  }

  /* setters */
  // themes
  updateThemeName(id, value) {
    const pos = this.themes.findIndex((x.id) == id);
    this.themes[pos].updateName(value);
  }

  updateThemeStatus(id, value) {
    const pos = this.themes.findIndex((x.id) == id);
    this.themes[pos].updateStatus(value);
  }

  updateThemeView(id, value) {
    const pos = this.themes.findIndex((x.id) == id);
    this.themes[pos].updateView(value);
  }

  updateThemeGroupGoal(id, value) {
    const pos = this.themes.findIndex((x.id) == id);
    this.themes[pos].updateGroupGoal(value);
  }

  updateThemeFeatureGoals(id, value) {
    const pos = this.themes.findIndex((x.id) == id);
    this.themes[pos].updateFeatureGoals(value);
  }

  updateThemeFeatureStatuses(id, value) {
    const pos = this.themes.findIndex((x.id) == id);
    this.themes[pos].updateFeatureStatuses(value);
  }

  // weights
  updateWeightName(id, value) {
    const pos = this.weights.findIndex((x.id) == id);
    this.weights[pos].updateName(value);
  }

  updateWeightStatus(id, value) {
    const pos = this.weights.findIndex((x.id) == id);
    this.weights[pos].updateStatus(value);
  }

  updateWeightFactor(id, value) {
    const pos = this.weights.findIndex((x.id) == id);
    this.weights[pos].updateFactor(value);
  }

  /* render */
  render() {
    // themes
    const theme_panel = this.container.querySelector(".themes");
    this.themes.forEach((x) => theme_panel.appendChild(x.render()))

    // weights
    const weight_panel = this.container.querySelector(".weights");
    this.weights.forEach((x) => weight_panel.appendChild(x.render()));
  }
}
