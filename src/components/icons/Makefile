tcastle2dcontrol.png: tcastle2dcontrol.xcf
	xcf2png $< $@

castleicons.lrs: *.png
	lazres castleicons.lrs *.png

.PHONY: clean
clean:
	rm -f castleicons.lrs
