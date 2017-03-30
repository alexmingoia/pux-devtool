const { Panel } = require('dev/panel');
const { Tool } = require('dev/toolbox');
const { Class } = require('sdk/core/heritage');
const self = require('sdk/self');
const tabs = require('sdk/tabs');

const { MessageChannel } = require('sdk/messaging');
const channel = new MessageChannel();
const addonSide = channel.port1;
const panelSide = channel.port2;

const PuxPanel = Class({
  extends: Panel,
  label: 'Pux',
  tooltip: 'Pux debugger',
  icon: self.data.url('128.png'),
  invertIconForDarkTheme: true,
  url: self.data.url('panel.html'),
  onReady: function() {
    var worker;
    var _ = this;

    function makeWorker (tab) {
      worker = tab.attach({
        contentScriptFile: self.data.url('content.js')
      })

      worker.port.on('pux:state:change', function (ev) {
        _.postMessage(JSON.stringify(ev));
      });

      worker.port.emit('init');
    }

    tabs.on('pageshow', makeWorker);

    addonSide.onmessage = function (ev) {
      if (worker) {
        worker.port.emit(ev.data);
      }
    };

    _.postMessage('init', [panelSide]);

    makeWorker(tabs.activeTab);
  }
});

exports.PuxPanel = PuxPanel;

const puxTool = new Tool({
  panels: { puxPanel: PuxPanel }
});
