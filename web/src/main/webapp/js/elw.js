/*
 * ELW : e-learning workspace
 * Copyright (C) 2010  Anton Kraievoy
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

var elw_enterTimes = {};
var elw_visible = {};
var elw_ajaxStamp = 0;
var elw_updateExpandTriggersUri = null; 

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
	if (elw_ajaxStamp > myAjaxStamp || elw_updateExpandTriggersUri == null) {
		return;
	}

	jQuery.ajax({
		"type": "POST",
		"url": elw_updateExpandTriggersUri,
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

function elw_dt_url(idx, inner) {
 return function(oObj) { return '<a href="'+oObj.aData[idx]+'">'+inner+'</a>'; };
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

 var jQexpand = jQuery(".expandTrigger");
 jQexpand.mouseenter(function() {
		elw_enterTimes[this.id] = new Date().getTime();
		setTimeout("elw_mouseHovered('"+this.id+"')", 600);
	});
	jQexpand.mouseleave(function() {
		elw_enterTimes[this.id] = null;
	});
	jQuery(".elw_dialogTrigger").click(function() {
  var thisJQ = jQuery(this);
  jQuery("."+thisJQ.attr("id")).dialog({
			modal: true,
			minWidth: 600,
			minHeight: 400,
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
  jQuery(this).find("input[type=submit]").attr("disabled", "true");
  jQuery(this).find("button[type=submit]").attr("disabled", "true");
	});
});
