#!/bin/bash
set -eu

TEXTURE_SIZE=2048
TEXTURE_SIZE_X=$TEXTURE_SIZE
TEXTURE_SIZE_Y=$TEXTURE_SIZE
AA_TEX_SIZE=1024

MAIN_PATH=`pwd`/../../
WORK_PATH="$MAIN_PATH"devel/textures/
FINAL_VRMLS_PATH="$MAIN_PATH"vrml/
FINAL_TEXTURES_PATH="$FINAL_VRMLS_PATH"textures/
FINAL_IMAGE_NAME="$FINAL_TEXTURES_PATH"base_shadowed.png

QUAD_Z=0.5

do_part ()
{
  case "$1" in
    mk_rgbe)
      cp -f "$WORK_PATH"white_pixel.png "$FINAL_IMAGE_NAME"

      # cd to $FINAL_VRMLS_PATH -- this way gen_light_map reads from stdin
      # and sees good directory structure (textures/, skies/, included
      # VRMLs files are where they are expected)
      cd "$FINAL_VRMLS_PATH"

      gen_light_map base.wrl "$WORK_PATH"base_shadowed.rgbe \
        $TEXTURE_SIZE_X $TEXTURE_SIZE_Y \
        -4.55 -4.57 $QUAD_Z 5.45 -4.57 $QUAD_Z \
        5.45 4.23 $QUAD_Z -4.55 4.23 $QUAD_Z \
        0 0 1
      ;;

    mk_final)
      # current dir does not matter here

      # Moglibysmy po prostu wrzucic base_shadowed.rgbe jako "$FINAL_IMAGE_NAME"
      # Nie robimy tego. Po pierwsze, wolimy skonwertowac rgbe na png aby
      # zajmowal mniej miejsca. Po drugie, chcemy najpierw nieco zmniejszyc
      # obrazek przy uzyciu jakiegos dobrego filtra ktore mamy dostepne
      # w convert ImageMagicka. Innymi slowy, chcemy zrobic anti-aliasing
      # texturze - OpenGL wprawdzie umie zrobic filtr GL_LINEAR teksturze
      # ale rezultaty wypadaja dosc blado. Zamiast ladowac obrazek
      # 2048x2048 do OpenGLa i polegac tylko na filtrowaniu OpenGLa
      # duzo lepsze rezultaty uzyskujemy skalujac obrazek najpierw do
      # 1024x1024 z jakims dobrym filtrem (uzywam Gaussian - jest lepszy
      # nawet od domyslnego, Lanchos). No i w ten sposob nie polegamy
      # tak bardzo na fakcie ze implementacja OpenGLa umie poradzic sobie
      # z teksturami tak duzego rozmiaru.

      # imgConvert sluzy mi tylko do konwersji rgbe->png.
      # Potem skalowanie obrazka robie przy pomocy convert.
      TEMP_IMAGE_NAME=/tmp/base_shadowed-$$.png
      imgConvert "$WORK_PATH"base_shadowed.rgbe "$TEMP_IMAGE_NAME"
      convert "$TEMP_IMAGE_NAME" -filter Gaussian \
	-geometry "${AA_TEX_SIZE}x${AA_TEX_SIZE}" "$FINAL_IMAGE_NAME"
      rm -f "$TEMP_IMAGE_NAME"
      ;;
    *)
      echo "Invalid 1st param for do_part: $1"
      exit 1
      ;;
  esac
}

if [ "$#" = 0 ]; then
  do_part mk_rgbe
  do_part mk_final
else
  do_part "$1"
fi
