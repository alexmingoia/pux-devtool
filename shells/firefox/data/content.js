self.port.on('init', function () {
  window.addEventListener('pux:state:change', function (ev) {
    self.port.emit('pux:state:change', ev.detail);
  });

  window.dispatchEvent(new CustomEvent('pux:devtool:init'));
});

self.port.on('first', function () {
  window.dispatchEvent(new CustomEvent('pux:state:first'))
});

self.port.on('prev', function () {
  window.dispatchEvent(new CustomEvent('pux:state:prev'))
});

self.port.on('next', function () {
  window.dispatchEvent(new CustomEvent('pux:state:next'))
});

self.port.on('last', function () {
  window.dispatchEvent(new CustomEvent('pux:state:last'))
});
