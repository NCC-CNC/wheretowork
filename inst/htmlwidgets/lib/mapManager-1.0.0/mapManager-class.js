class MapManager {
  /* constructor */
  constructor(id, container, layers, visible, order) {
    // set container
    this.id = id,
    this.container = container;
    // initialize layers
    this.layers = layers.map((x, i) => newLayer(id, x, visible[i]));
    this.order = order;
  }

  /* update method */
  updateLayer(id, parameter, value) {
    // TODO
  }

  /* render method */
  render() {
    const layers_panel = this.container.querySelector(".layers");
    this.layers.forEach((x) => layers_panel.appendChild(x.render()))
  }
}
