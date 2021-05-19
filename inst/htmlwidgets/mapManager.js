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

      renderValue: function(opts) {
        // alias this
        var that = this;

        // initialize widget
        if (!initialized) {
          // set state to initialized
          initialized = true;
          // attach the widget to the DOM
          container.widget = that;
          // initialize map manager
          handle = new MapManager(
            elementId, container, opts.layers, opts.visible, opts.order);
          // render HTML elements
          handle.render();
        }

      },

      resize: function(width, height) {
        // widget automatically resizes
      },

      // export object for extensibility
      mapManager: container,

      /* API functions to manipulate widget */
      updateLayer: function(params) {
        handle.updateLayer(
          params.value.id, params.value.parameter,
          params.value.value);
      },

      addLayer: function(params) {
        // TODO
      },

      dropLayer: function(params) {
        // TODO
      },

    };
  }
});

// Attach message handlers if in Shiny mode (these correspond to API)
if (HTMLWidgets.shinyMode) {
  var fxns = ["updateLayer", "addLayer", "removeLayer"];

  var addShinyHandler = function(fxn) {
    return function() {
      Shiny.addCustomMessageHandler(
        "solutionSettings:" + fxn, function(message) {
          var el = document.getElementById(message.id);
          if (el) {
            delete message["id"];
            el.widget[fxn](message);
          }
        }
      );
    }
  };

  for (var i = 0; i < fxns.length; ++i) {
    addShinyHandler(fxns[i])();
  }
}
