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
          // initialize solution settings manaer
          handle = new SolutionSettings(
            elementId, container, opts.themes, opts.weights);
          // render HTML elements
          handle.render();
          // over-write container height
          let raw_h = container.style.height;
          container.style.height = "auto";
          // set max height for themes and weights containters
          let h = `calc((${raw_h} - 17.6px) / 2)`;

          console.log(h);

          container
          .querySelectorAll(
            ".solution-settings .themes, .solution-settings .weights")
          .forEach((x) => x.style.maxHeight = h);

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
  var fxns = ["updateSetting"];

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
