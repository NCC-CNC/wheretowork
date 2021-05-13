HTMLWidgets.widget({

  name: 'mapManager',

  type: 'output',

  factory: function(el, width, height) {

    // shared variables
    var elementId = el.id;
    var initialized = false;
    var container = document.getElementById(elementId);
    var handle = undefined;

    return {

      renderValue: function(x) {
        // alias this
        var that = this;

        // initialize widget
        if (!initialized) {
          // set state to initialized
          initialized = true;
          // attach the widget to the DOM
          container.widget = that;
          // initialize solution settings manaer
          handle = new MapManager(
            elementId, container,
            opts.ids, opts.visible, opts.order, opts.legend);
          // render HTML elements
          handle.render();
        }

      },

      resize: function(width, height) {
        // widget automatically resizes
      }

      // export object for extensibility
      mapManager: container,

      /* API functions to manipulate widget */
      updateLayer: function(params) {
        handle.updateLayer(
          params.value.id, params.value.parameter,
          params.value.value);
      }

      addLayer: function(params) {
        handle.addLayer(
          params.value.id, );
      }

      updateSetting: function(params) {
        handle.updateLayer(
          params.value.id, params.value.parameter,
          params.value.value);
      }



    };
  }
});
