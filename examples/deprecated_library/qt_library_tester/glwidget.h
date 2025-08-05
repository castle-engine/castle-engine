#ifndef GLWIDGET_H
#define GLWIDGET_H

#include <QOpenGLWindow>

class MainWindow;

/* Having OpenGL rendering done inside the QWidget enables storing it to QLayouts or use
 * another widgets along it (like toolbar).
 *
 * While we could use QOpenGLWidget as a base class for our GLWidget, there are issues
 * in the initialization. It is done using offscreen buffer with different format, and
 * thus CGE detects wrong values in its initialization.
 *
 * So, instead, we use QOpenGLWindow (using native OpenGL surface from start)
 * and Qt windowContainer to ecapsulate QOpenGLWindow to a widget.
 *
 * In more detail:
 * QOpenGLWidget::initializeGL() is done with offscreen framebuffer, that does not have
 * multisampling switched on, even when multisampling is used in final rendering.
 * This is demostrated in PrintContextInfo() function that prints the value of GL_SAMPLES.
 *
 * We call Castle Engine initialization from initializeGL(), that sets TGLFeatures.CurrentMultiSampling.
 * While both GL_SAMPLE_BUFFERS and GL_SAMPLES are zero in QOpenGLWidget::initializeGL, Castle Engine
 * will discard multisampling in ScreenEffect shaders.
 */

class GLWidget : public QOpenGLWindow
{
    Q_OBJECT
public:
    explicit GLWidget(const QSurfaceFormat &format, MainWindow *parent = 0);
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
    MainWindow *m_pMainWnd;
    bool m_bAfterInit;
    bool m_bNeedsDisplay;
    bool m_bPrintContextInfoAtPaint;

protected:
    void initializeGL() override;
    void paintGL() override;
    void resizeGL(int width, int height) override;
    void mousePressEvent(QMouseEvent *event) override;
    void mouseMoveEvent(QMouseEvent *event) override;
    void mouseReleaseEvent(QMouseEvent *event) override;
#ifndef QT_NO_WHEELEVENT
    void wheelEvent(QWheelEvent *event) override;
#endif
    void keyPressEvent(QKeyEvent *event) override;
    void keyReleaseEvent(QKeyEvent *event) override;

private slots:
    void OnUpdateTimer();
};

#endif // GLWIDGET_H
