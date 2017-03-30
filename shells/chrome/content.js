window.addEventListener('pux:state:change', function (ev) {
  chrome.runtime.sendMessage({
    name: 'pux:state:change',
    detail: ev.detail
  })
});

window.dispatchEvent(new CustomEvent('pux:devtool:init'));

chrome.runtime.onMessage.addListener(function (message) {
  if (message === 'init') {
    window.dispatchEvent(new CustomEvent('pux:devtool:init'));
  }

  if (message === 'first') {
    window.dispatchEvent(new CustomEvent('pux:state:first'));
  }

  if (message === 'prev') {
    window.dispatchEvent(new CustomEvent('pux:state:prev'));
  }

  if (message === 'next') {
    window.dispatchEvent(new CustomEvent('pux:state:next'));
  }

  if (message === 'last') {
    window.dispatchEvent(new CustomEvent('pux:state:last'));
  }
})
