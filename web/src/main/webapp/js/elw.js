var elw_enterTimes = {};
var elw_visible = {};

function elw_mouseHovered(elemId) {
	if (elw_enterTimes[elemId] === undefined || elw_enterTimes[elemId] == null) {
		return;
	}
	elw_enterTimes[elemId] = null;

	if (elw_visible[elemId] === undefined || !elw_visible[elemId]) {
		jQuery("." + elemId).slideDown(0);
		elw_visible[elemId] = true;
	} else {
		jQuery("." + elemId).slideUp(0);
		jQuery("[class^=" + elemId + "]").slideUp(0);
		elw_visible[elemId] = false;
	}
}

jQuery(document).ready(function() {
	jQuery(".expandTrigger").mouseenter(function() {
		elw_enterTimes[this.id] = new Date().getTime();
		setTimeout("elw_mouseHovered('"+this.id+"')", 600);
	});
	jQuery(".expandTrigger").mouseleave(function() {
		elw_enterTimes[this.id] = null;
	});
	jQuery(".popupTrigger").mouseenter(function() {
		jQuery("." + this.id).slideDown(0);
	});
	jQuery(".popupTrigger").mouseleave(function() {
		jQuery("." + this.id).slideUp(0);
		jQuery("[class^=" + this.id + "]").slideUp(0);
	});
});
