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
    // find layer container
    const layers_panel = this.container.querySelector(".layers");
    // append layers to container
    this.layers.forEach((x) => layers_panel.appendChild(x.render()));
    // extract ids for each layer
    const ids = this.layers.map((x) => x.id);
    const n = ids.length + 1;
    /// alias this
    const that = this
    // enable sorting within container
    new Sortable(
      layers_panel, {
      animation: 150,
      dataIdAttr: "data-id",
      ghostClass: "map-manager-layer-ghost",
      onUpdate: function(event) {
        if (HTMLWidgets.shinyMode) {
          const new_ids = this.toArray();
          const order = ids.map((x) => n - (new_ids.findIndex((z) => z === x)));
          Shiny.setInputValue(that.id, {
            parameter: "layer_order",
            value: order
          });
        }
      }
    });
  }
}
