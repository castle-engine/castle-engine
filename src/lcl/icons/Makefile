.PHONY: all
all: *.png castleicons.lrs

tcastle2dcontrol.png: tcastle2dcontrol.xcf
	xcf2png $< -o $@

castleicons.lrs: *.png
	lazres castleicons.lrs *.png

.PHONY: clean
clean:
	rm -f castleicons.lrs
