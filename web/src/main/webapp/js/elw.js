var elw_enterTimes = {};
var elw_visible = {};

function elw_expand(elemId) {
	jQuery("#" + elemId).addClass("expanded");
	jQuery("." + elemId).slideDown(0);
	elw_visible[elemId] = true;
}

function elw_mouseHovered(elemId) {
	if (elw_enterTimes[elemId] === undefined || elw_enterTimes[elemId] == null) {
		return;
	}
	elw_enterTimes[elemId] = null;

	if (elw_visible[elemId] === undefined || !elw_visible[elemId]) {
		elw_expand(elemId);
	} else {
		jQuery("." + elemId).slideUp(0);
		jQuery("#" + elemId).removeClass("expanded");
		elw_visible[elemId] = false;
		//	process dependent triggers, if any
		for (var visElemId in elw_visible) {
			if (visElemId.indexOf(elemId) == 0 && elw_visible[visElemId]) {
				jQuery("." + visElemId).slideUp(0);
				jQuery("#" + visElemId).removeClass("expanded");
				elw_visible[visElemId] = false;
			}
		}
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
	jQuery("form[method=POST]").submit(function() {
		var visElemIds = [];
		for (var visElemId in elw_visible) {
			if (elw_visible[visElemId]) {
				visElemIds.push(visElemId);
			}
		}

		alert(jQuery.toJSON(visElemIds));
		jQuery(this).children("input[type=hidden][name=expandTriggers]").val(jQuery.toJSON(visElemIds));
	});
});
