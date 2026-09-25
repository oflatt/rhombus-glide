# Driving a running LibreOffice, for `raco glide`.
#
# LibreOffice has no command-line reload. The editor is launched with a UNO
# socket and this helper reloads the document that is already open, preserving
# the slide in view.
import sys
import time


def connect(port):
    import uno

    local = uno.getComponentContext()
    resolver = local.ServiceManager.createInstanceWithContext(
        "com.sun.star.bridge.UnoUrlResolver", local
    )
    return resolver.resolve(
        "uno:socket,host=localhost,port=%s;urp;StarOffice.ComponentContext" % port
    )


def find(ctx, url):
    desktop = ctx.ServiceManager.createInstanceWithContext(
        "com.sun.star.frame.Desktop", ctx
    )
    docs = desktop.getComponents().createEnumeration()
    while docs.hasMoreElements():
        doc = docs.nextElement()
        try:
            if doc.getURL() == url:
                # Disposed documents can linger in the enumeration.
                doc.getDrawPages().getCount()
                return doc
        except Exception:
            pass
    return None


def main(argv):
    if len(argv) != 4:
        sys.stderr.write("usage: libreoffice.py reload|open <port> <path>\n")
        return 2
    what, port, path = argv[1], argv[2], argv[3]
    import unohelper

    url = unohelper.systemPathToFileUrl(path)
    try:
        ctx = connect(port)
    except Exception:
        return 3
    doc = find(ctx, url)
    if doc is None:
        return 4
    if what == "open":
        return 0

    page_index = None
    try:
        current = doc.getCurrentController().getCurrentPage()
        pages = doc.getDrawPages()
        for i in range(pages.getCount()):
            if pages.getByIndex(i) == current:
                page_index = i
                break
    except Exception:
        pass

    # Dispatch from the frame itself. DispatchHelper accepts this command but
    # can silently do nothing.
    from com.sun.star.util import URL as UnoURL

    transformer = ctx.ServiceManager.createInstanceWithContext(
        "com.sun.star.util.URLTransformer", ctx
    )
    command = UnoURL()
    command.Complete = ".uno:Reload"
    _, command = transformer.parseStrict(command)
    frame = doc.getCurrentController().getFrame()
    dispatch = frame.queryDispatch(command, "_self", 0)
    if dispatch is None:
        return 5
    dispatch.dispatch(command, ())

    # Reload is asynchronous. Keep the bridge alive until the replacement
    # component exists; exiting immediately can cancel the request.
    fresh = None
    for _ in range(60):
        time.sleep(0.25)
        candidate = find(ctx, url)
        if candidate is not None and candidate != doc:
            fresh = candidate
            break
    if fresh is None:
        return 5
    if page_index is not None:
        try:
            pages = fresh.getDrawPages()
            if page_index < pages.getCount():
                fresh.getCurrentController().setCurrentPage(
                    pages.getByIndex(page_index)
                )
        except Exception:
            pass
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
