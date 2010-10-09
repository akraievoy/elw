package elw.web;

import elw.dao.Ctx;
import elw.vo.FileSlot;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.StringWriter;

public class EditorMips implements Editor {
	protected long cacheBustingToken = System.currentTimeMillis();

	public String render(HttpServletRequest req, HttpServletResponse resp, Ctx ctx, FileSlot slot) {
		final StringWriter out = new StringWriter();
		final String upHeader = "JSESSIONID=" + req.getSession(true).getId();

		out.write("<object classid=\"clsid:8AD9C840-044E-11D1-B3E9-00805F499D93\" width=\"800\" height=\"600\">");
		out.write("\t<param name=\"type\" value=\"application/x-java-applet;version=1.5\"/>");
		out.write("\t<param name=\"code\" value=\"elw.dp.ui.Applet\"/>");
		out.write("\t<param name=\"archive\" value=\"../code/" + cacheBustingToken + "/datapath-gui-1.3-full.jar\"/>");
		out.write("\t<param name=\"upHeader\" value=\"" + upHeader + "\"/>");
		out.write("\t<param name=\"elw_ctx\" value=\"" + ctx + "\"/>");
		out.write("\t<comment>");
		out.write("\t\t<embed width=\"800\" height=\"600\"");
		out.write("\t\t\t\ttype=\"application/x-java-applet;version=1.5\" pluginspage=\"http://java.sun.com/j2se/1.5.0/download.html\"");
		out.write("\t\t\t\tcode=\"elw.dp.ui.Applet\"");
		out.write("\t\t\t\tarchive=\"../code/" + cacheBustingToken + "/datapath-gui-1.3-full.jar\"");
		out.write("\t\t\t\tupHeader=\"" + upHeader + "\"");
		out.write("\t\t\t\telw_ctx=\"" + ctx + "\"");
		out.write("\t\t\t<noembed>");
		out.write("\t\t\t\tNo Java Support.");
		out.write("\t\t\t</noembed>");
		out.write("\t\t</embed>");
		out.write("\t</comment>");
		out.write("</object>");

		return out.toString();
	}
}