// define HTML widgets interface
HTMLWidgets.widget({
  name: "solutionSettings",
  type: "output",
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
          // initialize solution settings manager
          console.log(opts);
          handle = new SolutionSettings(
            elementId, container, opts.themes, opts.weights, opts.includes);
          // render HTML elements
          handle.render();
        }
      },

      resize: function(width, height) {
        // widget automatically resizes
      },

      // export object for extensibility
      solutionSettings: container,

      /* API functions to manipulate widget */
      updateSetting: function(params) {
        handle.updateSetting(
          params.value.id, params.value.parameter,
          params.value.value, params.value.type);
      }

    };
  }
});

// Attach message handlers if in Shiny mode (these correspond to API)
if (HTMLWidgets.shinyMode) {
  var fxns = ["update"];

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
