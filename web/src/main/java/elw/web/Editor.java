package elw.web;

import elw.dao.Ctx;
import elw.vo.FileSlot;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public interface Editor {
	String render(HttpServletRequest req, HttpServletResponse resp, final Ctx ctx, final FileSlot slot);
}
