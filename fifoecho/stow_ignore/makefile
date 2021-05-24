NAME := fifoecho
TARGET := $(INSTALLDIR)/$(NAME)

CFLAGS := -Wall -Wextra -pedantic -O3

ifndef INSTALLDIR
$(error INSTALLDIR not set)
endif

install: $(TARGET)

ifdef force
.PHONY: $(TARGET)
endif

$(TARGET): $(NAME).c
	$(CC) $(CFLAGS) $^ -o $(NAME)
	mv -f $(NAME) $@
