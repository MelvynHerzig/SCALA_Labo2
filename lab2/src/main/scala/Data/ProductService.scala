package Data

/**
  * Custom exception class used when an invalid brand is used for a given product.
  *
  * @param msg Message to be displayed.
  */
class InvalidBrandException(msg: String) extends Exception(msg) {}

trait ProductService:
  /**
    * Alias, a brand name is a String
    */
  type BrandName = String

  /**
    * Alias, a product name is a string
    */
  type ProductName = String

  /**
    * Gets the price of a given product.
    *
    * @param product Product name.
    * @param brand   Brand name.
    * @return Returns the price of the product.
    */
  def getPrice(product: ProductName, brand: BrandName): Double

  /**
    * Gets the default brand of a given product.
    *
    * @param product Product name.
    * @return Returns the default brand name of the product.
    */
  def getDefaultBrand(product: ProductName): BrandName

end ProductService


class ProductImpl extends ProductService :

  /**
    * Default brand and prices map for beers.
    */
  val beer: ProductInformation = ProductInformation("boxer", Map(
    "farmer" -> 1.0,
    "boxer" -> 1.0,
    "wittekop" -> 2.0,
    "punkipa" -> 3.0,
    "jackhammer" -> 3.0,
    "tenebreuse" -> 4.0
  ))

  /**
    * Default brand and prices map for croissants
    */
  val croissant: ProductInformation = ProductInformation("maison", Map(
    "maison" -> 2.0,
    "cailler" -> 2.0
  ))

  /**
    * For a given product gets the information associated.
    *
    * @param product Product name.
    * @return Returns the default brand and the map of price.
    */
  private def getProductInformations(product: ProductName): ProductInformation =
    product match
      case "croissant" => croissant
      case "biere" => beer
      case _ => throw new IllegalArgumentException("Unknown product")
  end getProductInformations

  def getPrice(product: ProductName, brand: String): Double =
    if brand == null then
      throw new IllegalArgumentException("Brand can not be null")
    else
      getProductInformations(product).getPrice(brand)
  end getPrice

  def getDefaultBrand(product: ProductName): BrandName =
    getProductInformations(product).getDefaultBrand
  end getDefaultBrand

end ProductImpl

/**
  * Class in charge of storing product information.
  *
  * @param defaultBrand Default brand name of the product.
  * @param prices       Default prices for each brand of the product.
  */
class ProductInformation(val defaultBrand: String, val prices: Map[String, Double]):

  /**
    * Alias, a brand name is a String
    */
  type BrandName = String

  /**
    * Gets the price of the product for the given brand. If the brand is "", default brand is used.
    *
    * @param brand Brand name.
    * @return Returns the price of the product for the given brand.
    * @throws InvalidBrandException when the brand doesn't exist for the product.
    */
  def getPrice(brand: BrandName): Double =
    if brand.isEmpty then
      prices(getDefaultBrand)
    else if !prices.contains(brand) then
      throw new InvalidBrandException("Invalid brand name for this product")
    else prices(brand)
  end getPrice

  /**
    * Gets the default brand of a product.
    *
    * @return Returns the brand name.
    */
  def getDefaultBrand: BrandName = defaultBrand

end ProductInformation
