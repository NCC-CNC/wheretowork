class Statistic {
  /* constructor */
  constructor(
    manager,
    name,
    value,
    units) {
    // initialize HTML element to display the results
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".statistic-template")
        .content,
      true);

    // append statistics to the HTML element
    const l = document.createElement("label");
    l.innerText = `${name}: ${value} ${units}`;
    this.el.appendChild(l);

  }

  /* render method */
  render() {
    return this.el;
  }

};