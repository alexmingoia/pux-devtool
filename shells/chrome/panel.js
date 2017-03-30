var eventEl = document.getElementById("event");
var stateEl = document.getElementById("state");
var statesEl = document.getElementById("states");
var prevEl = document.getElementById("prev");
var nextEl = document.getElementById("next");
var firstEl = document.getElementById("first");
var lastEl = document.getElementById("last");

firstEl.addEventListener("click", function (ev) {
  window.postMessage('first', '*');
});

prevEl.addEventListener("click", function (ev) {
  window.postMessage('prev', '*');
});

nextEl.addEventListener("click", function (ev) {
  window.postMessage('next', '*');
});

lastEl.addEventListener("click", function (ev) {
  window.postMessage('last', '*');
});

window.addEventListener('message', function (ev) {
  if (ev.data && ev.data.name) {
    if (ev.data.name === 'pux:state:change') {
      eventEl.textContent = ev.data.detail.event
      statesEl.textContent = (ev.data.detail.index + 1) + ' / ' + ev.data.detail.length;
      stateEl.innerHTML = '';
      stateEl.appendChild((new JSONFormatter(JSON.parse(ev.data.detail.state))).render());
    }
  }
})
