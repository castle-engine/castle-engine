#ifndef GLWIDGET_H
#define GLWIDGET_H

#include <QOpenGLWidget>

class GLWidget : public QOpenGLWidget
{
    Q_OBJECT
public:
    explicit GLWidget(const QSurfaceFormat &format, QWidget *parent = 0);
    ~GLWidget();

    QString m_sSceneToOpen;
    bool m_bLimitFPS;
    void OpenScene(QString const &sFilename);

    // helpers
public:
    static int QKeyToCgeKey(int qKey);

protected:
    static int OpenGlLibraryCallback(int eCode, int iParam1, int iParam2, const char *szParam);
    QPoint PointFromMousePoint(const QPoint& pt);
    QPoint PointFromMousePoint(const QPointF& pt);
    void PrintContextInfo(const QString &sTitle);

private:
    bool m_bAfterInit;
    bool m_bNeedsDisplay;
    bool m_bPrintContextInfoAtPaint;

public:
    virtual QSize minimumSizeHint() const;
    virtual QSize sizeHint() const;

protected:
    virtual void initializeGL();
    virtual void paintGL();
    virtual void resizeGL(int width, int height);
    virtual void mousePressEvent(QMouseEvent *event);
    virtual void mouseMoveEvent(QMouseEvent *event);
    virtual void mouseReleaseEvent(QMouseEvent *event);
#ifndef QT_NO_WHEELEVENT
    virtual void wheelEvent(QWheelEvent *event);
#endif
    virtual void keyPressEvent(QKeyEvent *event);
    virtual void keyReleaseEvent(QKeyEvent *event);

private slots:
    void OnUpdateTimer();
};

#endif // GLWIDGET_H
