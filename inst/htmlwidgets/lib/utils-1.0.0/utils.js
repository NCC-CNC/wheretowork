// round a number to a certain number of digits
function roundToDigits(x, digits) {
  return +(Math.round(x + `e+${digits}`)  + `e-${digits}`);
}

// automatically round a number
function auto_round(x) {
  // initialize output
  let out = x;
  const y = Math.abs(x);
  // round number according to rules of thumb
  if (y >= 100) {
    // round to integer
    out = roundToDigits(x, 0);
  } else if (y >= 10) {
    // round to one decimal place
    out = roundToDigits(x, 1);
  } else if (y >= 1) {
    // round to two decimal places
    out = roundToDigits(x, 2);
  } else {
    // round to three decimal places
    out = roundToDigits(x, 3);
  }
  // return result
  return out;
}

// obtained from
// https://www.javascripttutorial.net/dom/manipulating/remove-all-child-nodes/
function removeAllChildNodes(parent) {
    while (parent.firstChild) {
        parent.removeChild(parent.firstChild);
    }
}

// obtained form
// https://stackoverflow.com/a/63418989/3483791
const getUniqueBy = (arr, prop) => {
  const set = new Set;
  return arr.filter(o => !set.has(o[prop]) && set.add(o[prop]));
};

// create description icon with the popover description window
function createDescription(el, name, description) {
    const icon_el = document.createElement("a");
    icon_el.setAttribute("data-toggle", "popover");
    icon_el.setAttribute("data-container", "body");
    icon_el.setAttribute("data-trigger", "focus");
    icon_el.setAttribute("html", "true");
    icon_el.setAttribute("title", name + "<a href='javascript:void(0);' class='close'>&times;</a>");
    icon_el.setAttribute("data-content", description);
    icon_el.classList.add("fas");
    icon_el.classList.add("fa-info-circle");
    icon_el.style.color = "#118ab2";
    $(icon_el).popover({
        html: true, sanitize: false
    });
    // add click action to icon
    icon_el.addEventListener("click", function() {
        $(this).popover("toggle");
    });
    // hide popover on close button
    $(icon_el).on("shown.bs.popover", function() {
        $(".popover").on("click", ".close", function() {
            $(icon_el).popover("hide");
        });
    });
    el.appendChild(icon_el);
}

// create data provenance icon
function createProvenance(el, provenance) {
  const icon_el = document.createElement("i");
  icon_el.setAttribute("data-toggle", "tooltip");
  icon_el.setAttribute("data-placement", "top");
  icon_el.setAttribute("data-container", "body");
  icon_el.setAttribute("title", provenance.description);
  if (provenance.icon === "canadian-maple-leaf") {
    icon_el.classList.add("fab");
  } else {
    icon_el.classList.add("fas");
  }
  icon_el.classList.add("fa-" + provenance.icon);
  icon_el.style.color = provenance.color;
  el.appendChild(icon_el);
}
