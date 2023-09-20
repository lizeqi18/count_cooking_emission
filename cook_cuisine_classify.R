# Categorize restaurants' cuisine
rm(list=ls())
setwd(dir="/home/rstudio/data/cook_poi")
library(readr)
library(data.table)

#input 2021 cook poi data
cy21new<-read_csv("2021-cook.csv", locale = locale(encoding = "GBK"))#784
head(cy21new)
cy21=cy21new[,c(1,4,5,6,13,15,23,24)] #Filter out the required columns

KK<-cy21 # KK is a data frame for dealing with categorization of cuisines
head(KK)
colnames(KK)<-c("name","primary_lable","second_label","tertiary_label", 
                "province","city","longitude","latitude") # Convert column headings to English 

cuisine<-rep("Unclassified",nrow(KK))
KK<-data.frame(cuisine,KK)
head(KK)

# Use word frequency analysis to filter out words that occur frequently in the names of restaurants of a certain type of cuisine
library(jiebaR)
  # Take Sichuan & Hunan cuisine for example: 
  name1<-KK[grep(pattern="湖南菜|四川菜",KK[,5]),2] # Those dishes whose tertiary label includes "川菜" or "湘菜"  belong to Sichuan-Hunan cuisine
  wk<-worker()
  fq<-freq(wk[name1])
  fq<-fq[order(-fq$freq),]
  head(fq,n=100) # obtain the keywords that most frequently appear in the names of restaurants of this cuisine
# Next, we manually screen out those keywords unique to Sichuan and Hunan cuisine,
# i.e., exclude keywords relevant to multiple cuisines
# and also add some specific keywords based on our experience.

# Similarly, list the specific keywords corresponding to all cuisines, as shown in Table S4
  # Catering services without organic emissions
  name1<-KK[grep(pattern="咖啡|茶艺|冷饮|糕饼|甜品",KK[,5]),2] 
  wk<-worker()
  fq<-freq(wk[name1])
  fq<-fq[order(-fq$freq),]
  head(fq,n=100)

  # Hotpot
  name1<-KK[grep(pattern="火锅|呷哺呷哺",KK[,5]),2] 
  wk<-worker()
  fq<-freq(wk[name1])
  fq<-fq[order(-fq$freq),]
  head(fq,n=100)
  
  # Cantonese-Fujian cuisine
  name1<-KK[grep(pattern="广东菜|福建菜|潮州菜",KK[,5]),2] 
  wk<-worker()
  fq<-freq(wk[name1])
  fq<-fq[order(-fq$freq),]
  head(fq,n=100) 
  
  # Jiangsu-Zhejiang cuisine
  name1<-KK[grep(pattern="江苏菜|浙江菜",KK[,5]),2] 
  wk<-worker()
  fq<-freq(wk[name1])
  fq<-fq[order(-fq$freq),]
  head(fq,n=100) 
  
  # non-Chinese cuisine
  name1<-KK[grep(pattern="外国餐厅|西餐厅(综合风味)|日本料理|韩国料理|法式菜品餐厅|
  + 意式菜品餐厅|泰国/越南菜品餐厅|地中海风格菜品|美式风味|印度风味|英国式菜品餐厅|
  + 牛扒店(扒房)|俄国菜|葡国菜|德国菜|巴西菜|墨西哥菜|其它亚洲菜|肯德基|麦当劳|必胜客|吉野家",KK[,5]),2] 
  wk<-worker()
  fq<-freq(wk[name1])
  fq<-fq[order(-fq$freq),]
  head(fq,n=100) 
  
  # Chinese Fast Food & Snacks
  name1<-KK[grep(pattern="快餐厅|永和豆浆|大家乐|大快活|美心",KK[,5]),2] 
  wk<-worker()
  fq<-freq(wk[name1])
  fq<-fq[order(-fq$freq),]
  head(fq,n=100) 
  
  # Other Chinese cuisine
  name1<-KK[grep(pattern="山东菜|上海菜|安徽菜|北京菜|湖北菜|东北菜|云贵菜|西北菜|台湾菜|清真菜馆",KK[,5]),2] 
  wk<-worker()
  fq<-freq(wk[name1])
  fq<-fq[order(-fq$freq),]
  head(fq,n=100)   
  
  # Notably, barbecue does not correspond to specific labels, we list its keywords based on our experience
  
# Next, sequentially screen out restaurants with labels, keywords specific to a certain cuisine (listed in Table S4) as the order in column 5 of Table S4
  # Since the later classified ones will overwrite the first classified ones, the code order here uses the reverse order of the order in Table S4 to ensure the priority of the later
  # Other Chinese cuisine
  KK[grep(pattern="山东菜|上海菜|安徽菜|北京菜|湖北菜|东北菜|云贵菜|西北菜|台湾菜|清真菜馆|地方风味",KK[,5]),1]<-"other Chinese cuisine"
  KK[grep(pattern="山东|北京|鲁|济南|胶东|徽|淮南|皖北|新疆|上海|沪|湖北|鄂|东北|云贵|云南|贵州|滇|台湾烤鸭|葱烧|酱爆|扒鸡|锅塌|夹饼|板面|清真|回族|回民|京味|西北菜",KK[,2]),1]<-"other Chinese cuisine"
  aggregate(name~cuisine,KK,length)  
  
  # Chinese Fast Food & Snacks
  KK[grep(pattern="快餐厅|永和豆浆|大家乐|大快活|美心",KK[,5]),1]<-"Chinese Fast Food and Snacks"
  KK[grep(pattern="饺|馄饨|抄手|面粉|拉面|面馆|凉皮|粉馆|米线|米粉|刀削面|包子|肉夹馍|煎饼|烧饼|馒头|肉饼|大饼|熟食|粥|炒饭|豆浆|快餐|小吃|早餐|早点",KK[,2]),1]<-"Chinese Fast Food and Snacks"
 
  # non-Chinese cuisine
  KK[grep(pattern="外国餐厅|西餐厅(综合风味)|日本料理|韩国料理|法式菜品餐厅|意式菜品餐厅|
  + 泰国/越南菜品餐厅|地中海风格菜品|美式风味|印度风味|英国式菜品餐厅|牛扒店(扒房)|俄国菜|
  + 葡国菜|德国菜|巴西菜|墨西哥菜|其它亚洲菜|肯德基|麦当劳|必胜客|吉野家",KK[,5]),1]<-"non-Chinese cuisine"
  KK[grep(pattern="寿司|披萨|料理|炸鸡|比萨|牛排|汉堡|沙拉|外国|日式|韩式|西餐|国际|意式",KK[,2]),1]<-"non-Chinese cuisine"
   
  # Jiangsu-Zhejiang cuisine
  KK[grep(pattern="江苏菜|浙江菜",KK[,5]),1]<-"Jiangsu-Zhejiang cuisine"
  KK[grep(pattern="杭州|无锡|淮扬|浙|绿茶餐厅",KK[,2]),1]<-"Jiangsu-Zhejiang cuisine"
  
  # Cantonese-Fujian cuisine
  KK[grep(pattern="广东菜|福建菜|潮州菜|茶餐厅",KK[,5]),1]<-"Cantonese-Fujian cuisine"
  KK[grep(pattern="粤|广东|潮汕|闽|沙县|潮州|客家|莆田|砂锅粥|烧腊|肠粉|烧鹅|早茶|潮味|港式|原味汤粉王",KK[,2]),1]<-"Cantonese-Fujian cuisine"

  # Sichuan-Hunan cuisine
  KK[grep(pattern="湖南菜|四川菜",KK[,5]),1]<-"Sichuan-Hunan cuisine"
  KK[grep(pattern="川|蜀|重庆|渝|湘|湖南|长沙|浏阳|湘西|成都|麻辣|香辣|小面|鸡公煲|酸菜鱼|酸辣粉",KK[,2]),1]<-"Sichuan-Hunan cuisine"

  # hotpot
  KK[grep(pattern="火锅|呷哺呷哺",KK[,5]),1]<-"hotpot"
  KK[grep(pattern="火锅|串串香|铜锅|麻辣烫|涮|海底捞",KK[,2]),1]<-"hotpot"
  
  # barbecue
  KK[grep(pattern="铁板烧|串烧|烧烤|炭火|果木",KK[,2]),1]<-"barbecue"

  # Catering services without organic emissions  
  KK[grep(pattern="咖啡|茶艺|冷饮|糕饼|甜品",KK[,5]),1]<-"Catering services without organic emissions"
  KK[grep(pattern="奶茶|咖啡|冷饮|冰淇淋|饮品|星巴克|酸奶|水吧|味多美|蛋糕|烘焙|甜品|糕点|面包|西饼|稻香村|好利来|茶楼|茶馆|茶艺|茶坊|凉茶|茶庄|茶园|水果",KK[,2]),1]<-"Catering services without organic emissions"
  aggregate(name~cuisine,KK,length)
  
  # The remaining unclassified restaurants are classified as home-style cuisine
    KK[grep(pattern="Unclassified",KK[,1]),1]<-"home-style cuisine"
  
  # Home-style cuisines in these provinces has distinctive local characteristics and are also categorized as the corresponding local cuisines.
    KK[which(KK$cuisine=="home-style cuisine"&KK$province=="北京市"),1]<-"other Chinese cuisine"
    KK[which(KK$cuisine=="home-style cuisine"&KK$province=="山东省"),1]<-"other Chinese cuisine"
    KK[which(KK$cuisine=="home-style cuisine"&KK$province=="安徽省"),1]<-"other Chinese cuisine"
    KK[which(KK$cuisine=="home-style cuisine"&KK$province=="新疆维吾尔自治区"),1]<-"other Chinese cuisine"
    KK[which(KK$cuisine=="home-style cuisine"&KK$province=="江苏省"),1]<-"Jiangsu-Zhejiang cuisine"
    KK[which(KK$cuisine=="home-style cuisine"&KK$province=="浙江省"),1]<-"Jiangsu-Zhejiang cuisine"
    KK[which(KK$cuisine=="home-style cuisine"&KK$province=="广东省"),1]<-"Cantonese-Fujian cuisine"
    KK[which(KK$cuisine=="home-style cuisine"&KK$province=="福建省"),1]<-"Cantonese-Fujian cuisine"
    KK[which(KK$cuisine=="home-style cuisine"&KK$province=="香港特别行政区"),1]<-"Cantonese-Fujian cuisine"
    KK[which(KK$cuisine=="home-style cuisine"&KK$province=="湖南省"),1]<-"Sichuan-Hunan cuisine"
    KK[which(KK$cuisine=="home-style cuisine"&KK$province=="四川省"),1]<-"Sichuan-Hunan cuisine"
    KK[which(KK$cuisine=="home-style cuisine"&KK$province=="重庆市"),1]<-"Sichuan-Hunan cuisine"
    
    # Exclude the canteens.
     KK<-KK[-grep(pattern="食堂",KK[,2]),]
     head(KK)

    # Export the cuisines of all restaurants
    write.csv(KK,"cooking_point_cuisine.csv")
    