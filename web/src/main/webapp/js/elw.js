var elw_enterTimes = {};
var elw_visible = {};
var elw_ajaxStamp = 0;

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

	elw_ajaxStamp++;
	setTimeout("elw_ajaxStart(" + elw_ajaxStamp + ")", 10000);
}

function elw_expandedTriggers() {
	var visElemIds = [];
	for (var visElemId in elw_visible) {
		if (elw_visible[visElemId]) {
			visElemIds.push(visElemId);
		}
	}
	return visElemIds;
}

function elw_ajaxStart(myAjaxStamp) {
	if (elw_ajaxStamp > myAjaxStamp) {
		return;
	}

	jQuery.ajax({
		"type": "POST",
		"url": "updateExpandTriggers",
		"data": jQuery.toJSON(elw_expandedTriggers()),
		"success": function(resp) {
			if (resp == null || !resp.success) {
				jQuery.jGrowl("Failed to save view state", {"header":"Warning", "life": 10000});
			} else {
				jQuery.jGrowl("View state saved", {"header":"Info", "life": 3000});
			}
		},
		"error": function() {
			jQuery.jGrowl("Failed to save view state", {"header":"Warning", "life": 10000});
		},
		"dataType": 'json'
	});
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
		var visElemIds = elw_expandedTriggers();
		jQuery(this).children("input[type=hidden][name=expandTriggers]").val(jQuery.toJSON(visElemIds));
	});
});
