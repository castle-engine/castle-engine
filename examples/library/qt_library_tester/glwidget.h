#ifndef GLWIDGET_H
#define GLWIDGET_H

#include <QGLWidget>

class GLWidget : public QGLWidget
{
    Q_OBJECT
public:
    explicit GLWidget(QWidget *parent = 0);
    ~GLWidget();

    void OpenScene(QString const &sFilename);

private:
    QString m_sSceneToOpen;
    bool m_bAfterInit;

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

private slots:
    void OnUpdateTimer();
};

#endif // GLWIDGET_H
