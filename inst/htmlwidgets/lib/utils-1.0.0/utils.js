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

// create include/exclude overalp warning icon
function createWarning(el, name, layer, overlap) {
  const icon_el = document.createElement("i");
  icon_el.setAttribute("data-toggle", "tooltip");
  icon_el.setAttribute("data-placement", "top");
  icon_el.setAttribute("data-container", "body");
  const warning_msg = name + ' overlaps the following ' + layer +': ' + overlap + '.' 
  icon_el.setAttribute("title", warning_msg);
  icon_el.classList.add("fa");
  icon_el.classList.add("fa-exclamation-triangle");
  icon_el.style.color = "#eed202";
  icon_el.style.marginRight = "10px";
  el.appendChild(icon_el);
}

// create file icon
function createFile(el) {
  const icon_el = document.createElement("i");
  icon_el.setAttribute("data-toggle", "tooltip");
  icon_el.setAttribute("data-placement", "bottom");
  icon_el.setAttribute("data-container", "body");
  icon_el.setAttribute("title", "No .yaml file submitted.");
  icon_el.classList.add("fa");
  icon_el.classList.add("fa-file");
  icon_el.style.color = "#b8b8b8";
  icon_el.style.marginRight = "10px";
  el.appendChild(icon_el);
}
