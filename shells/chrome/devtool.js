chrome.devtools.panels.create('Pux', '128.png', 'panel.html', function (panel) {
  const backgroundPort = chrome.runtime.connect({name: 'pux'});

  panel.onShown.addListener(function (panelWindow) {
    backgroundPort.postMessage({
        name: 'init',
        tabId: chrome.devtools.inspectedWindow.tabId
    });

    panelWindow.addEventListener('message', function (ev) {
      backgroundPort.postMessage({
        name: ev.data,
        tabId: chrome.devtools.inspectedWindow.tabId
      })
    })

    backgroundPort.onMessage.addListener(function(message) {
      panelWindow.postMessage(message, '*')
    })
  });
});
