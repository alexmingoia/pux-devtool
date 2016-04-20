// module Pux.Devtool

exports.actionToString = function (a) {
  function toString(a) {
    var name = a.constructor.name.match(/(String|Number)/) ? a : a.constructor.name;
    var str = [name];
    Object.keys(a).forEach(function (key) {
      if (key[0] === 'v' && key[4] === 'e') {
        str.push('(' + toString(a[key]) + ')');
      }
    });
    return str.join(' ');
  }

  return toString(a);
};

exports.stateToString = function (s) {
  return JSON.stringify(s, function (key, val) {
    if (!val.constructor.name.match(/(Object|String|Number|Date|Symbol)/)) {
      return exports.actionToString(val);
    }
    return val;
  }, 2)
  .replace(/"([^"]+)":/g, '$1:')
  .replace(/<\//g, '<\\/')
  .replace(/\n/g, '<br />')
  .replace(/  /g, '&nbsp;&nbsp;');
};
