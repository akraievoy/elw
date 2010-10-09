var elw_enterTimes = {};
var elw_visible = {};
var elw_ajaxStamp = 0;

function elw_expand(elemId) {
	var trigger = jQuery("#" + elemId);

	trigger.addClass("expanded");
	if (trigger.hasClass("ui-icon-triangle-1-e")) {
		trigger.removeClass("ui-icon-triangle-1-e");
		trigger.addClass("ui-icon-triangle-1-s");
	}
	jQuery("." + elemId).slideDown(0);
	

	elw_visible[elemId] = true;
}

function elw_collapse(elemId) {
	jQuery("." + elemId).slideUp(0);
	var trigger = jQuery("#" + elemId);
	trigger.removeClass("expanded");
	if (trigger.hasClass("ui-icon-triangle-1-s")) {
		trigger.removeClass("ui-icon-triangle-1-s");
		trigger.addClass("ui-icon-triangle-1-e");
	}

	elw_visible[elemId] = false;
}

function elw_mouseHovered(elemId) {
	if (elw_enterTimes[elemId] === undefined || elw_enterTimes[elemId] == null) {
		return;
	}
	elw_enterTimes[elemId] = null;

	if (elw_visible[elemId] === undefined || !elw_visible[elemId]) {
		elw_expand(elemId);
	} else {
		elw_collapse(elemId);
		//	process dependent triggers, if any
		for (var visElemId in elw_visible) {
			if (visElemId.indexOf(elemId) == 0 && elw_visible[visElemId]) {
				elw_collapse(visElemId);
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
	jQuery(".elw_button").button();
	jQuery(".elw_button_info").button({
		icons: {
			primary: "ui-icon-info"
		}
	});
	jQuery(".elw_button_alert").button({
		icons: {
			primary: "ui-icon-alert"
		}
	});
	jQuery(".elw_button_logout").button({
		icons: {
			primary: "ui-icon-power"
		}
	});
	jQuery(".elw_button_login").button({
		icons: {
			primary: "ui-icon-unlocked"
		}
	});
	jQuery(".elw_button_edit").button({
		icons: {
			primary: "ui-icon-pencil"
		}
	});
	jQuery(".elw_button_upload").button({
		icons: {
			primary: "ui-icon-arrowstop-1-n"
		}
	});

	jQuery(".expandTrigger").mouseenter(function() {
		elw_enterTimes[this.id] = new Date().getTime();
		setTimeout("elw_mouseHovered('"+this.id+"')", 600);
	});
	jQuery(".expandTrigger").mouseleave(function() {
		elw_enterTimes[this.id] = null;
	});
	jQuery(".elw_dialogTrigger").click(function() {
  var thisJQ = jQuery(this);
  thisJQ.next(".elw_dialogContent:first").clone().dialog({
			modal: true,
			minWidth: 600,
			title: thisJQ.text()
		});
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
