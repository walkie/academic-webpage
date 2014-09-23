function mailTo(address) {
  return '<a href="mailto:' + address + '">' + address + '</a>';
}

function setEmail() {
  // some encoded strings
  var user = '&#119;&#97;&#108;&#107;&#105;&#110;&#101;&#114;&#64;';
  var osu  = '&#101;&#101;&#99;&#115;&#46;&#111;&#114;&#101;&#103;&#111;&#110;&#115;&#116;&#97;&#116;&#101;&#46;&#101;&#100;&#117;';
  var osuList = '&#64;&#101;&#110;&#103;&#114;&#46;&#111;&#114;&#115;&#116;&#46;&#101;&#100;&#117;';
  var marburg = '&#105;&#110;&#102;&#111;&#114;&#109;&#97;&#116;&#105;&#107;&#46;&#117;&#110;&#105;&#45;&#109;&#97;&#114;&#98;&#117;&#114;&#103;&#46;&#100;&#101;';

  // insert my email address
  $('.osu-email').html(mailTo(user + osu));
  $('.marburg-email').html(mailTo(user + marburg));
  
  // insert OSU mailing list address
  $('.osu-mailing-list')
    .html(
      function(i,name) {
        return mailTo(name + osuList);
      }
    );
}

function linkName() {
  $('.my-name').html('<a href="http://web.engr.oregonstate.edu/~walkiner/">Eric Walkingshaw</a>');
}

function toggleAbstract(key) {
  $('.pub-abstract.'+key).slideToggle();
}
