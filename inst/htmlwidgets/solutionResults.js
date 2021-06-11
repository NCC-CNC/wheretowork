HTMLWidgets.widget({
  name: "solutionResults",
  type: "output",
  factory: function(el, width, height) {
    // shared variables
    var elementId = el.id;
    var initialized = false;
    var container = document.getElementById(elementId);
    var handle = undefined;

    return {

      renderValue: function(opts) {
        // initialize widget
        if (!initialized) {
          // set state to initialized
          initialized = true;
          // attach the widget to the DOM
          container.widget = this;
          // initialize map manager
          handle = new SolutionResults(elementId, container);
          // render HTML elements
          handle.render();
          // if opts contains any data, then add solutions
          if (opts.solutions.length > 0) {
            opts.solutions.forEach((x) => {
              const s = newSolution(elementId, x);
              handle.addSolution(s);
            });
            handle.showSolution(opts.solutions[0].id);
          }
        }

      },

      resize: function(width, height) {
        // widget automatically resizes
      },

      // export object for extensibility
      solutionResults: container,

      /* API functions to manipulate widget */
      addSolution: function(params) {
        const solution = newSolution(elementId, params.value);
        handle.addSolution(solution);
      },

      dropSolution: function(params) {
        handle.dropSolution(params.value);
      },

      showSolution: function(params) {
        handle.showSolution(params.value);
      }

    };
  }
});

// Attach message handlers if in Shiny mode (these correspond to API)
if (HTMLWidgets.shinyMode) {
  var fxns = ["addSolution", "dropSolution", "addSolution", "showSolution"];

  var addShinyHandler = function(fxn) {
    return function() {
      Shiny.addCustomMessageHandler(
        "solutionResults:" + fxn, function(message) {
          var el = document.getElementById(message.id);
          console.log("el");
          console.log(el);
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
