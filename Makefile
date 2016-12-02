#-------------------------start--------------------------------
VERSION=1.1.1 #首先定义版本

.PHONY: deps rel
# 默认为获取依赖 OTP 项目，然后进行编译


IP=`ifconfig|grep "inet "|grep -v "127.0.0.1"|cut -d: -f2|awk '{print $$2}'`

all: deps compile 

help:
	@echo
	@echo "Usage: "
	@echo "    make {compile|clean}"
	@echo "    make {rel|package}"
	@echo "    make {run}"
	@echo
	@echo

#编译相关项目，在编译之前先查看依赖项目是否已经存在
compile:deps
	./rebar compile

#获取 OTP 项目
deps:
	./rebar get-deps
clean:
	./rebar clean

#清除依赖项目
distclean: clean
	./rebar delete-deps

rel:
	sed -i "s/127.0.0.1/$(IP)/g" ./rel/files/vm.args
	cd ./rel/ && ../rebar generate

run:
	./rel/bibi/bin/bibi start

#-------------------------end-----------------------------------