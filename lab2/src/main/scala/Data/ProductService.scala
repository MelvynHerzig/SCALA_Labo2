package Data


trait ProductService:
  type BrandName = String
  type ProductName = String

  def getPrice(product: ProductName, brand: BrandName): Double

  def getDefaultBrand(product: ProductName): BrandName
end ProductService


class ProductImpl extends ProductService :

  val beer: ProductInformations = ProductInformations("boxer", Map(
    "farmer" -> 1.0,
    "boxer" -> 1.0,
    "wittekop" -> 2.0,
    "punkipa" -> 3.0,
    "jackhammer" -> 3.0,
    "tenebreuse" -> 4.0
  ))

  val croissant: ProductInformations = ProductInformations("maison", Map(
    "maison" -> 2.0,
    "cailler" -> 2.0
  ))

  private def doOperationForProduct(product: ProductName) : ProductInformations =
    product match {
      case "croissant" => croissant
      case "bière" => beer
    }

  // TODO - Part 2 Step 2
  def getPrice(product: ProductName, brand: String): Double =


  def getDefaultBrand(product: ProductName): BrandName = ???
end ProductImpl

class ProductInformations(var defaultBrand: String, var prices: Map[String, Double]):
  type BrandName = String

  def getPrice(brand: BrandName): Double = prices(brand)

  def getDefaultBrand: BrandName = defaultBrand
end ProductInformations
