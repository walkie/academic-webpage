function mailTo(address) {
  return '<a href="mailto:' + address + '">' + address + '</a>';
}

function setEmail() {
  var user = '&#119;&#97;&#108;&#107;&#105;&#110;&#101;&#114;&#64;';
  var osu = '&#101;&#101;&#99;&#115;&#46;&#111;&#114;&#101;&#103;&#111;&#110;&#115;&#116;&#97;&#116;&#101;&#46;&#101;&#100;&#117';
  var marburg = '&#105;&#110;&#102;&#111;&#114;&#109;&#97;&#116;&#105;&#107;&#46;&#117;&#110;&#105;&#45;&#109;&#97;&#114;&#98;&#117;&#114;&#103;&#46;&#100;&#101;';
  $('.osu-email').html(mailTo(user + osu));
  $('.marburg-email').html(mailTo(user + marburg));
}

function toggleAbstract(key) {
  $('.pub-abstract.'+key).slideToggle();
}
