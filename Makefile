#-------------------------start--------------------------------
VERSION=1.1.1 #首先定义版本

.PHONY: deps rel
# 默认为获取依赖 OTP 项目，然后进行编译


IP=`ifconfig|grep "inet addr"|grep -v "127.0.0.1"|cut -d: -f2|awk '{print $$1}'`

all: co

help:
	@echo
	@echo "说明: "
	@echo "    make co 		编译"
	@echo "    make rel		打包"
	@echo "    make run		运行"
	@echo "    make clean		清除"
	@echo "    make distclean  	清除依赖项目"
	@echo
	@echo

#编译相关项目，在编译之前先查看依赖项目是否已经存在
co:deps
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

onekey: co rel
	./rel/bibi/bin/bibi console


#-------------------------end-----------------------------------