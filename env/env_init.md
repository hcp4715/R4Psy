# 环境准备

> 最后修改时间：**2025年3月8日11点07分**
>
> 贡献者：[ShaeRay](https://github.com/ShaeRay)

## Git

### 1. 安装包下载

> 以win10为例

https://git-scm.com/downloads/win

选择 64-bit Git for Windows Setup

![image-20250307190757209](./assets/image-20250307190757209.png)

### 2. 安装

![image-20250307192748213](./assets/image-20250307192748213.png)

👇安装位置默认即可，路径中不要含有中文

![image-20250307192816626](./assets/image-20250307192816626.png)

一路按下Next，不需要修改任何配置

出现下面这个界面，不需要修改，按下【Finish】就安装完成

![image-20250307193527231](./assets/image-20250307193527231.png)

### 3. 验证

如图所示。出现git version 2.48.1.windows.1 就是安装成功了

![image-20250307193818104](./assets/image-20250307193818104.png)

### 4. 配置

- 在windows自带的搜索框中打开【Git Bash】

![image-20250307194156303](./assets/image-20250307194156303.png)

- 配置用户名（Your Name替换成自己的）

  ```bash
  git config --global user.name "Your Name"
  ```

- 配置邮箱（email@example .com替换成自己的）

  ```bash
  git config --global user.email "email@example.com"
  ```

  > Git中无法使用【CTRL+V】粘贴
  >
  > 粘贴单击右键，点【Paste】

  ![image-20250307194420511](./assets/image-20250307194420511.png)

- 验证是否配置成功

  ```bash
  git config --global --list
  ```

  > name和邮箱出来就可以，其他配置不一样不用管

![image-20250307194745078](./assets/image-20250307194745078.png)

- 生成SSH key

  ```bash
  ssh-keygen -t rsa -C “上面的邮箱”
  ```

  > 输入后一直回车

![image-20250307195053105](./assets/image-20250307195053105.png)

![image-20250307195105952](./assets/image-20250307195105952.png)

- 现在去获取公钥，依次执行下面两个命令，复制得到的结果（ssh开头，邮箱结尾）

  > git里右键点击【copy】才可以复制

```bash
cd ~/.ssh
```

```bash
cat id_rsa.pub
```

![image-20250307195348204](./assets/image-20250307195348204.png)

- 打开Github。依次点击【头像】、【Settings】、【SSH and GPG  keys】

![](./assets/image-20250307195510975.png)

- New SSH key

![image-20250307195606871](./assets/image-20250307195606871.png)

- Tille随便写一个，Key里粘贴上面得到的结果，【Add SSH key】

![image-20250307195724695](./assets/image-20250307195724695.png)

- 下面这样就是成功了

![image-20250307195917406](./assets/image-20250307195917406.png)

- 验证

  ```bash
  ssh -T git@github.com
  ```

  > 输入yes

![image-20250307195949093](./assets/image-20250307195949093.png)

- 这样就是成功了（南师大本校的同学如果失败，把校园网关了切热点）

  ![image-20250307200855073](./assets/image-20250307200855073.png)

## R

https://mirrors.tuna.tsinghua.edu.cn/CRAN/

### 1. 安装

![image-20250307202508101](./assets/image-20250307202508101.png)

- 所有配置全部默认，不做任何修改，一路下一步

![image-20250307202620224](./assets/image-20250307202620224.png)

### 2. 验证

- Windows默认搜索，R

![image-20250307202722154](./assets/image-20250307202722154.png)

- 能打开就是成功了

![image-20250307202831490](./assets/image-20250307202831490.png)

## R Studio

### 1. 安装

- 安装路径不要有中文就行，没有什么配置项，直接安装

![image-20250307203407124](./assets/image-20250307203407124.png)

![image-20250307203547558](./assets/image-20250307203547558.png)

- 打开RStudio

![image-20250307203659882](./assets/image-20250307203659882.png)

- 用前面装好的64位R即可

![image-20250307203745989](./assets/image-20250307203745989.png)

## R4psy

- fork老师的仓库

  > https://github.com/hcp4715/R4Psy

![image-20250307204126256](./assets/image-20250307204126256.png)

- 默认即可，【Create Fork】

![image-20250307204214450](./assets/image-20250307204214450.png)



- 左上角【File】、【New Project】、【Version Control】

![image-20250308103154502](./assets/image-20250308103154502.png)

- 选【Git】

![image-20250308103214321](./assets/image-20250308103214321.png)

- 去github复制自己的仓库SSH地址

![image-20250308110119700](./assets/image-20250308110119700.png)

- 回来粘贴到【URL】里，注意文件夹不要包含中文

![image-20250308103239969](./assets/image-20250308103239969.png)

- 下载完成后右侧会出现老师仓库的文件夹和一个【GIT】

![image-20250308104245304](./assets/image-20250308104245304.png)

- 在【/homeworks/2025】文件夹下创建自己的文件后，会出现在【Git】下，勾选

![image-20250308104544472](./assets/image-20250308104544472.png)



- 在【Commit message】区域随便填，然后【commit】

![image-20250308104655642](./assets/image-20250308104655642.png)

- 没什么问题就【close】

![image-20250308104619551](./assets/image-20250308104619551.png)

- 点击【push】，没什么问题就push成功了

![image-20250308104726895](./assets/image-20250308104726895.png)

- 回到github，会显示有commit，点击

![image-20250308105505033](./assets/image-20250308105505033.png)

- 创建pull request

![image-20250308105526108](./assets/image-20250308105526108.png)

- 填完信息，大功告成

## 常见问题

### 1. 电脑中文用户名无法创建SSH密钥

- ① 在C盘用户文件夹下新建一个 .ssh 文件夹

  ![image-20250310221357043](./assets/image-20250310221357043.png)

- ② 以管理员权限打开git bash

![image-20250310221500039](./assets/image-20250310221500039.png)

- ③ 添加参数-f 指定文件夹

  ```bash
  ssh-keygen -t rsa -C "邮箱" -f "C:/Users/.ssh/id_rsa"
  ```

- 在C盘/用户/.ssh文件夹下找到【id_rsa.pub】文件，以txt格式打开并复制

  ![image-20250310221652036](./assets/image-20250310221652036.png)

- 接上续教程👉打开Github。依次点击【头像】、【Settings】、【SSH and GPG  keys】