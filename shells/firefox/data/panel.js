var eventEl = document.getElementById('event');
var stateEl = document.getElementById('state');
var statesEl = document.getElementById('states');
var prevEl = document.getElementById('prev');
var nextEl = document.getElementById('next');
var firstEl = document.getElementById('first');
var lastEl = document.getElementById('last');

firstEl.addEventListener('click', function (ev) {
  window.port.postMessage('first');
});

prevEl.addEventListener('click', function (ev) {
  window.port.postMessage('prev');
});

nextEl.addEventListener('click', function (ev) {
  window.port.postMessage('next');
});

lastEl.addEventListener('click', function (ev) {
  window.port.postMessage('last');
});

window.addEventListener('message', function (ev) {
  if (ev.data === 'init') {
    window.port = ev.ports[0];
    window.port.start();
  } else {
    var data = JSON.parse(ev.data);

    if (data.event) {
      eventEl.textContent = data.event
      statesEl.textContent = (data.index + 1) + ' / ' + data.length;
      stateEl.innerHTML = '';
      stateEl.appendChild((new JSONFormatter(JSON.parse(data.state))).render());
    }
  }
});
