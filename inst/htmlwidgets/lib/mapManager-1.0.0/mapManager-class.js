class MapManager {
  /* constructor */
  constructor(id, container, layers, order) {
    // set container
    this.id = id,
    this.container = container;
    // initialize layers
    this.layers = layers.map((x, i) => newLayer(id, x));
    this.order = order;
    this.sortable = undefined;
  }

  /* update method */
  updateLayer(id, parameter, value) {
    const idx = this.layers.findIndex((x) => x.id === id);
    this.layers[idx].updateParameter(parameter, value);
  }

  /* update method */
  updateOrder(value) {
    // create array with ids in order
    const ids = this.layers.map((x) => x.id);
    const new_ids = ids;
    for (let i = 0; i < new_ids.length; ++i) {
      new_ids[ids.length - value[i]] = ids[i];
    }
    // re-order layers in widget
    this.sortable.sort(new_ids, true);
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
    this.sortable = new Sortable(
      layers_panel, {
      animation: 150,
      dataIdAttr: "data-id",
      ghostClass: "map-manager-layer-ghost",
      onUpdate: function(event) {
        if (HTMLWidgets.shinyMode) {
          const new_ids = this.toArray();
          const order = ids.map((x) => n - (new_ids.findIndex((z) => z === x)));
          Shiny.setInputValue(that.id, {
            parameter: "order",
            value: order
          });
        }
      },
      onEnd: function(event) {
        $(".layers").find(".map-manager-layer").eq(event.newIndex).addClass('map-manager-layer-ghost')
        $("html").click(function(p){
          $(".layers").find(".map-manager-layer").removeClass('map-manager-layer-ghost')
        })
      }
    });
  }
}
