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

  /* methods */
  addLayer(value) {
    // create layer
    const x = newLayer(this.id, value);
    // update fields
    this.layers.push(x);
    this.order.push(Math.max.apply(null, this.order) + 1);
    // insert HTML element as the first layer
    this.container.querySelector(".layers").prepend(x.render());
  }

  dropLayer(id) {
    // find solution index
    const idx = this.layers.findIndex((x) => x.id === id);
    // remove layer
    if (idx > -1) {
      this.layers.splice(idx, 1);
      this.order.splice(idx, 1);
      this.sortable.el.querySelector(`#${this.id} [data-id='${id}']`).remove();
    }
  }

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
    /// alias this
    const that = this
    // enable sorting within container
    this.sortable = new Sortable(
      layers_panel, {
      animation: 150,
      dataIdAttr: "data-id",
      ghostClass: "ghost",
      onUpdate: function(event) {
        if (HTMLWidgets.shinyMode) {
          // extract ids for each layer
          const ids = that.layers.map((x) => x.id);
          const n = ids.length + 1;
          // extract re-order of ids
          const new_ids = this.toArray();
          // determine order of each id
          const order = ids.map((x) => n - (new_ids.findIndex((z) => z === x)));
          // send order to R session
          Shiny.setInputValue(that.id, {
            parameter: "order",
            value: order
          });
        }
      }
    });
  }
}
